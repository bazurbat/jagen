
local System = require 'System'
local Log    = require 'Log'

local Source = {}

function Source:new(o)
    local o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

local DistSource = Source:new()
local GitSource  = Source:new()
local HgSource   = Source:new()
local RepoSource = Source:new()

-- Source

function Source:is_scm()
    return self.type == 'git' or self.type == 'hg' or self.type == 'repo'
end

function Source:_basename(filename)
    local name = string.match(filename, '^.*/(.+)') or filename
    local exts = { '%.git', '%.tar', '%.tgz', '%.txz', '%.tbz2',
        '%.zip', '%.rar', ''
    }
    for _, ext in ipairs(exts) do
        local match = string.match(name, '^([%w_.-]+)'..ext)
        if match then
            return match
        end
    end
end

function Source:create(source, name)
    local source = source or {}

    -- Protect from double initialization, workaround for source.dir appending
    -- value below
    if getmetatable(source) then
        return source
    end

    if source.type == 'dist' then
        source = DistSource:new(source)
    elseif source.type == 'git' then
        source = GitSource:new(source)
    elseif source.type == 'hg' then
        source = HgSource:new(source)
    elseif source.type == 'repo' then
        source = RepoSource:new(source)
    else
        source = Source:new(source)
    end

    if source.location then
        if not source.filename then
            source.filename = string.match(source.location, '^.*/(.+)') or source.location
        end

        if not source.basename then
            source.basename = source:_basename(source.filename)
        end

        local base_dir = source.base_dir
        if not base_dir then
            if source:is_scm() then
                base_dir = '$jagen_src_dir'
            else
                base_dir = System.mkpath('$jagen_build_dir', name or source.basename)
            end
        end

        source.dir = System.mkpath(base_dir, source.dir or source.basename)
    end

    return source
end

function Source:fixup()
    return true
end

-- DistSource

function DistSource:new(o)
    local source = Source.new(DistSource, o)
    return source
end

-- GitSource

function GitSource:new(o)
    local source = Source.new(GitSource, o)
    source.branch = source.branch or 'master'
    return source
end

function GitSource:exec(command, ...)
    return System.exec('git -C "%s" '..command, self.dir, ...)
end

function GitSource:pread(format, command, ...)
    return System.pread(format, 'git -C "%s" '..command, self.dir, ...)
end

function GitSource:head()
    return self:pread('*l', 'rev-parse HEAD')
end

function GitSource:dirty()
    return self:pread('*l', 'status --porcelain') ~= nil
end

function GitSource:clean()
    return self:exec('checkout HEAD .') and self:exec('clean -fxd')
end

function GitSource:_resolve_ref(pattern)
    local ls = self:pread('*l', 'ls-remote -q --refs origin "%s"', pattern)
    if ls then
        return string.match(ls, '%S+%s+(%S+)')
    end
end

function GitSource:update()
    local ref = assert(self:_resolve_ref(self.branch))
    local refspec = string.format('+%s:%s', ref, ref)
    return self:exec('fetch --prune --no-tags origin "%s"', refspec)
end

function GitSource:_is_branch(pattern)
    local branch = self:pread('*l', 'branch -a --list "%s"', pattern)
    local exists, active = false, false

    if branch and #branch > 0 then
        exists = true
        active = string.sub(branch, 1, 1) == '*'
    end

    return exists, active
end

function GitSource:_is_tag(pattern)
    local tag = self:pread('*l', 'tag --list "%s"', pattern)
    local exists, active = false, false

    if tag and #tag > 0 then
        exists = true
    end

    return exists, active
end

function GitSource:_checkout()
    local branch = assert(self.branch)
    local exists, active = self:_is_branch(branch) or self:_is_tag(branch)
    if active then
        return true
    elseif exists then
        return self:exec('checkout "%s"', branch)
    else
        local start_point = 'origin/'..branch
        exists = self:_is_branch(start_point)
        if exists then
            return self:exec('remote set-branches origin "%s"', branch) and
                   self:exec('checkout -b "%s" "%s"', branch, start_point)
        else
            Log.error("could not find branch '%s' in local repository", branch)
            return false
        end
    end
end

function GitSource:_show_ref(pattern)
    local ls = self:pread('*l', 'show-ref "%s"', pattern)
    if ls then
        return string.match(ls, '%S+%s+(%S+)')
    end
end

function GitSource:_merge()
    local ref = assert(self:_show_ref(self.branch))
    return self:exec('merge --ff-only "%s"', ref)
end

function GitSource:switch()
    return self:_checkout() and self:_merge()
end

function GitSource:clone()
    return System.exec('git clone --depth 1 --branch "%s" "%s" "%s"',
        assert(self.branch), assert(self.location), assert(self.dir))
end

function GitSource:fixup()
    if self.assume_unchanged then
        return self:exec('update-index --assume-unchanged %s',
            System.quote(table.unpack(self.assume_unchanged)))
    end
    return true
end

-- HgSource

function HgSource:new(o)
    local source = Source.new(HgSource, o)
    source.branch = source.branch or 'default'
    return source
end

function HgSource:exec(command, ...)
    return System.exec('hg -R "%s" '..command, self.dir, ...)
end

function HgSource:pread(format, command, ...)
    return System.pread(format, 'hg -R "%s" '..command, self.dir, ...)
end

function HgSource:head()
    return self:pread('*l', 'id -i')
end

function HgSource:dirty()
    return self:pread('*l', 'status') ~= nil
end

function HgSource:clean()
    return self:exec('update -C "%s"', assert(self.branch)) and
           self:exec('purge --all')
end

function HgSource:update()
    return self:exec('pull -r "%s"', assert(self.branch))
end

function HgSource:_branch()
    local s = self:pread('*l', 'branch')
    if s then
        return string.match(s, '^[%w_-]+')
    end
end

function HgSource:_is_bookmark(pattern)
    local bm = self:pread('*a', "bookmarks | grep '^..."..pattern.."\\s'")
    local exists, active = false, false

    if bm and #bm > 0 then
        exists = true
        active = string.sub(bm, 2, 2) == '*'
    end

    return exists, active
end

function HgSource:switch()
    local branch = assert(self.branch)
    local exists = self:_is_bookmark(branch)
    if exists then
        return self:exec('update -r "%s"', branch)
    elseif branch == self:_branch() then
        return true
    else
        Log.error("could not find bookmark '%s' in local repository", branch)
        return false
    end
end

function HgSource:clone()
    return System.exec('hg clone -r "%s" "%s" "%s"',
        assert(self.branch), assert(self.location), assert(self.dir))
end

-- RepoSource

function RepoSource:new(o)
    local source = Source.new(RepoSource, o)
    source.jobs = Jagen.nproc * 2
    return source
end

function RepoSource:exec(command, ...)
    return System.exec('cd "%s" && repo '..command, assert(self.dir), ...)
end

function RepoSource:pread(format, command, ...)
    return System.pread(format, 'cd "%s" && repo '..command, assert(self.dir), ...)
end

function RepoSource:_load_projects(...)
    local o = {}
    local file = System.popen('cd "%s" && repo list', assert(self.dir))
    for line in file:lines() do
        local path, name = string.match(line, '(.+)%s:%s(.+)')
        if name then
            o[name] = path
        end
    end
    file:close()
    return o
end

function RepoSource:_is_dirty(path)
    return System.pread('*l', 'git -C "%s" status --porcelain', path) ~= nil
end

function RepoSource:head()
    return self:pread('*a', 'status -j1 --orphans')
end

function RepoSource:dirty()
    local projects = self:_load_projects()
    for n, p in pairs(projects) do
        local path = System.mkpath(assert(self.dir), p)
        if System.exists(path) and not System.is_empty(path)
                and self:_is_dirty(path) then
            return true
        end
    end
    return false
end

function RepoSource:clean()
    local projects = self:_load_projects()
    for n, p in pairs(projects) do
        local path = System.mkpath(assert(self.dir), p)
        if System.exists(path) then
            if self:_is_dirty(path) then
                return System.exec('git -C "%s" checkout HEAD .', path) and
                       System.exec('git -C "%s" clean -fxd', path)
            end
        end
    end
    return true
end

function RepoSource:update()
    return self:exec('sync -j%d --current-branch --no-tags --optimized-fetch',
        self.jobs)
end

function RepoSource:switch()
    -- Not doing Android development at the time.
    Log.warning('branch switching is not implemented for repo sources')
    return true
end

function RepoSource:clone()
    return System.exec('mkdir -p "%s"', self.dir) and
           self:exec('init -u "%s" -b "%s" -p linux --depth 1',
               assert(self.location), assert(self.branch)) and
           self:update()
end

return Source
