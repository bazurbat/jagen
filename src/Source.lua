
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

function GitSource:command(command, ...)
    return string.format('git --git-dir .git -C "%s" '..command, self.dir, ...)
end

function GitSource:exec(command, ...)
    return System.exec('git --git-dir .git -C "%s" '..command, self.dir, ...)
end

function GitSource:pread(format, command, ...)
    return System.pread(format, 'git --git-dir .git -C "%s" '..command, self.dir, ...)
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

function GitSource:_ls_remote(pattern)
    local function get_matching_remote_ref(line)
        if line then
            return assert(string.match(line, '%S+%s+(%S+)'))
        end
    end
    -- read_single_line will fail here if there are a branch and a tag with the
    -- same name, changed to reading first line but need to think of a way to
    -- better handle this case
    return System.pipe(
        get_matching_remote_ref,
        self:command('ls-remote --quiet --refs origin "%s"', pattern),
        io.read_line)
end

function GitSource:_ref_is_tag(ref)
    return string.match(ref, '/tags/')
end

function GitSource:update()
    local src = self:_ls_remote(self.branch)
    if not src then
        Log.error("could not find tag or branch '%s' in '%s'", self.branch, self.location)
        return false
    end
    local dst
    if self:_ref_is_tag(src) then
        dst = string.format('refs/tags/%s', self.branch)
    else
        dst = string.format('refs/remotes/origin/%s', self.branch)
    end
    local refspec = string.format('+%s:%s', src, dst)
    return self:exec('fetch --prune --no-tags origin "%s"', refspec)
end

function GitSource:switch()
    local branch = assert(self.branch)

    local function get_matching_ref(line)
        local ref, active = nil, false
        if line and #line > 0 then
            ref = string.match(line, '%s*(%S+)')
            active = string.sub(line, 1, 1) == '*'
        end
        return ref, active
    end

    local ref, active = System.pipe(
        get_matching_ref,
        self:command('branch --list "%s"', branch),
        io.read_single_line)

    if active then
        return true
    end

    if ref then
        return
            self:exec('checkout "%s"', branch) and
            self:exec('merge --ff-only "%s"', 'origin/'..branch) and
            self:exec('submodule update --init --recursive')
    end

    ref = System.pipe(
        get_matching_ref,
        self:command('branch --list --remotes "%s"', 'origin/'..branch),
        io.read_single_line)

    if ref then
        return
            self:exec('checkout -b "%s" "%s"', branch, ref) and
            self:exec('merge --ff-only "%s"', 'origin/'..branch) and
            self:exec('submodule update --init --recursive')
    end

    local tag = System.pipe(
        get_matching_ref,
        self:command('tag --list "%s"', branch),
        io.read_single_line)

    if tag then
        return
            self:exec('checkout "%s"', self.branch) and
            self:exec('submodule update --init --recursive')
    end

    Log.error("could not find tag or branch '%s' in '%s'", branch, self.dir)

    return false
end

function GitSource:clone()
    assert(self.location)
    assert(self.branch)
    assert(self.dir)
    local depth_arg
    -- http transport does not support depth
    if not string.match(self.location, '^http.*') then
        depth_arg = '--depth 1 '
    end
    local clone_res = System.exec('git clone %s --branch "%s" "%s" "%s"',
        depth_arg or '', self.branch, self.location, self.dir)
    -- Checkout of submodule might fail with depth if it is not enough to reach
    -- the referenced commit. Do the full checkout for now.
    return clone_res and self:exec('submodule update --init --recursive')
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
