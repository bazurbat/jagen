local Command = require 'Command'
local System = require 'System'
local Log    = require 'Log'

local format = string.format
local match = string.match

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
    local name, match = string.match(filename, '^.*/(.+)') or filename
    for _, ext in ipairs({ '%.tar%.%w+', '%.[^.]+' }) do
        match = string.match(name, string.format('^(.+)%s$', ext))
        if match then return match end
    end
    return name
end

function Source:create(source, name)
    local source = source or {}

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

    if not source.dir then
        source.dir = System.mkpath('$jagen_src_dir', name)
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

function GitSource:git(...)
    return Command:new('git -C', quote(self.dir), ...)
end

function GitSource:command(command, ...)
    return string.format('git --git-dir .git -C "%s" '..command, self.dir, ...)
end

function GitSource:exec(command, ...)
    return System.exec('git --git-dir .git -C "%s" '..command, self.dir, ...)
end

function GitSource:head()
    return self:git('rev-parse HEAD'):read()
end

function GitSource:dirty()
    return self:git('status --porcelain'):read() ~= nil
end

function GitSource:clean()
    return self:git('checkout HEAD .'):exec() and self:git('clean -fxd'):exec()
end

function GitSource:update_submodules()
    if not self.exclude_submodules and
            System.file_exists(System.mkpath(self.dir, '.gitmodules')) then
        return self:git('submodule update --init --recursive'):exec()
    end
    return true
end

function GitSource:update()
    local branch = self.branch
    if not branch then return false end
    local line = self:git('ls-remote --heads --tags origin', quote(branch)):read()
    if not line then
        Log.error("could not find tag or branch '%s' in '%s'", branch, self.location)
        return false
    end
    local src, dst = match(line, '^%S+%s+(%S+)$')
    local name = assert(match(src, '^.+/([^/]+)$'))
    if match(src, '/tags/') then
        dst = format('refs/tags/%s', name)
    else
        dst = format('refs/remotes/origin/%s', name)
    end
    local refspec = format('+%s:%s', src, dst)
    return self:git('fetch --prune origin', quote(refspec)):exec() and
           self:update_submodules()
end

function GitSource:switch()
    local branch = self.branch
    if not branch then return false end

    local name = self:git('branch --list', quote(branch)):read()
    if name then
        if string.sub(name, 1, 1) == '*' then -- already active
            return true
        else -- switch to matching local branch
            name = string.trim(name)
            return self:git('checkout -q', quote(name)):exec() and
                   self:git('merge --ff-only', quote('origin/'..name)):read() and
                   self:update_submodules()
        end
    end

    name = self:git('branch --list --remotes', quote('origin/'..branch)):read()
    if name then -- switch to a new remote branch
        name = assert(string.match(name, '^%s+origin/(%S+)'))
        return self:git('checkout -b', quote(name), quote('origin/'..name)):exec() and
               self:update_submodules()
    end

    local tag = self:git('tag --list', quote(branch)):read()
    if tag then
        return self:git('checkout', quote(tag)):exec() and
               self:update_submodules()
    end

    Log.error("could not find tag or branch '%s' in '%s'", branch, self.dir)

    return false
end

function GitSource:clone()
    local clone = Command:new('git clone --progress')
    if self.branch then
        clone:append('--branch', quote(self.branch))
    end
    -- try to detect if the server is "smart"
    -- https://stackoverflow.com/questions/9270488/is-it-possible-to-detect-whether-a-http-git-remote-is-smart-or-dumb
    local headers = Command:new('curl -si', quote(self.location..'/info/refs?service=git-upload-pack')):read('*a')
    if string.match(headers, 'Content%-Type: application/x%-git') then -- smart
        clone:append('--depth', 1)
    end
    clone:append('--', quote(self.location), quote(self.dir))

    local ok, status = clone:exec()
    if ok then return self:update_submodules() end
    return ok, status
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
    source.jobs = Jagen.nproc() * 2
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
