
local system = require 'system'

local Source = {}

function Source:new(o)
    local o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

local GitSource  = Source:new()
local HgSource   = Source:new()
local RepoSource = Source:new()

-- Source

function Source._read_line(file)
    return file:read()
end

function Source._read_all(file)
    return file:read('*a')
end

function Source:is_scm()
    return self.type == 'git' or self.type == 'hg' or self.type == 'repo'
end

function Source:basename(filename)
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

    if source.type == 'git' then
        source = GitSource:new(source)
    elseif source.type == 'hg' then
        source = HgSource:new(source)
    elseif source.type == 'repo' then
        source = RepoSource:new(source)
    elseif source.type == 'dist' then
        source.location = '$jagen_dist_dir/'..source.location
        source = Source:new(source)
    else
        source = Source:new(source)
    end

    if source.location and source.type ~= 'curl' then
        local basename = source:basename(source.location)
        local src_dir = assert(os.getenv('jagen_src_dir'))
        local work_dir = assert(os.getenv('jagen_build_dir'))
        local dir
        if source:is_scm() then
            dir = src_dir
        else
            dir = system.mkpath(work_dir, name or basename)
        end
        source.dir = system.mkpath(dir, source.dir or basename)
    end

    return source
end

function Source:fixup()
    return true
end

-- GitSource

function GitSource:new(o)
    local source = Source.new(GitSource, o)
    source.branch = source.branch or 'master'
    return source
end

function GitSource:exec(...)
    return system.exec('git', '-C', assert(self.dir), ...)
end

function GitSource:pread(format, command, ...)
    return system.pread(format, 'git -C "%s" '..command, self.dir, ...)
end

function GitSource:head()
    return self:pread('*l', 'rev-parse HEAD')
end

function GitSource:dirty()
    return self:pread('*l', 'status --porcelain') ~= nil
end

function GitSource:clean()
    return self:exec('checkout', 'HEAD', '.') and self:exec('clean', '-fxd')
end

function GitSource:update()
    local branch = assert(self.branch)
    local cmd = { 'fetch', '--prune', '--no-tags', 'origin',
        string.format('+refs/heads/%s:refs/remotes/origin/%s', branch, branch)
    }
    return self:exec(unpack(cmd))
end

function GitSource:_is_branch(pattern)
    local branch = self:pread('*l', 'branch -a --list', pattern)
    local exists, active = false, false

    if branch and #branch > 0 then
        exists = true
        active = string.sub(branch, 1, 1) == '*'
    end

    return exists, active
end

function GitSource:_checkout()
    local branch = assert(self.branch)
    local exists, active = self:_is_branch(branch)
    if active then
        return true
    elseif exists then
        return self:exec('checkout', branch)
    else
        local start_point = 'origin/'..branch
        exists = self:_is_branch(start_point)
        if exists then
            local add = { 'remote', 'set-branches', 'origin', branch }
            local checkout = { 'checkout', '-b', branch, start_point }
            return self:exec(unpack(add)) and self:exec(unpack(checkout))
        else
            jagen.error("could not find branch '%s' in local repository", branch)
            return false
        end
    end
end

function GitSource:_merge()
    local branch = assert(self.branch)
    local cmd = { 'merge', '--ff-only', string.format('origin/%s', branch) }
    return self:exec(unpack(cmd))
end

function GitSource:switch()
    return self:_checkout() and self:_merge()
end

function GitSource:clone()
    return system.exec('git', 'clone', '--branch', assert(self.branch),
        '--depth', 1, assert(self.location), assert(self.dir))
end

function GitSource:fixup()
    if self.assume_unchanged then
        return self:exec('update-index', '--assume-unchanged',
            unpack(self.assume_unchanged))
    end
    return true
end

-- HgSource

function HgSource:new(o)
    local source = Source.new(HgSource, o)
    source.branch = source.branch or 'default'
    return source
end

function HgSource:exec(...)
    return system.exec('hg', '-R', assert(self.dir), ...)
end

function HgSource:pread(format, command, ...)
    return system.pread(format, 'hg -R "%s" '..command, self.dir, ...)
end

function HgSource:head()
    return self:pread('*l', 'id -i')
end

function HgSource:dirty()
    return self:pread('*l', 'status') ~= nil
end

function HgSource:clean()
    return self:exec('update', '-C', assert(self.branch)) and
           self:exec('purge', '--all')
end

function HgSource:update()
    local cmd = { 'pull', '-r', assert(self.branch) }
    return self:exec(unpack(cmd))
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
        local cmd = { 'update', '-r', assert(self.branch) }
        return self:exec(unpack(cmd))
    elseif branch == self:_branch() then
        return true
    else
        jagen.error("could not find bookmark '%s' in local repository", branch)
        return false
    end
end

function HgSource:clone()
    return system.exec('hg', 'clone', '-r', assert(self.branch),
        assert(self.location), assert(self.dir))
end

-- RepoSource

function RepoSource:new(o)
    local source = Source.new(RepoSource, o)
    source.jobs = jagen.nproc * 2
    return source
end

function RepoSource:exec(...)
    local cmd = { 'cd', '"'..assert(self.dir)..'"', '&&', 'repo', ... }
    return system.exec(unpack(cmd))
end

function RepoSource:pread(format, command, ...)
    local cmd = { 'cd', '"'..assert(self.dir)..'"', '&&', 'repo', ... }
    return system.pread(unpack(cmd))
end

function RepoSource:_load_projects(...)
    local o = {}
    local list = self:popen('list', ...)
    while true do
        local line = list:read()
        if not line then break end
        local path, name = string.match(line, "(.+)%s:%s(.+)")
        if name then
            o[name] = path
        end
    end
    return o
end

function RepoSource:_is_dirty(path)
    local cmd   = string.format('git -C "%s" status --porcelain', path)
    local pipe  = assert(system.popen(cmd))
    local dirty = pipe:read() ~= nil
    pipe:close()
    return dirty
end

function RepoSource:head()
    return self:popen('status', '-j', 1, '--orphans'):read('*all')
end

function RepoSource:dirty()
    local projects = self:_load_projects()
    for n, p in pairs(projects) do
        local path = system.mkpath(assert(self.dir), p)
        if system.exists(path) and not system.is_empty(path)
                and self:_is_dirty(path) then
            return true
        end
    end
    return false
end

function RepoSource:clean()
    local projects = self:_load_projects()
    for n, p in pairs(projects) do
        local path = system.mkpath(assert(self.dir), p)
        if system.exists(path) then
            if self:_is_dirty(path) then
                local checkout = string.format('git -C "%s" checkout HEAD .', path)
                if not system.exec(checkout) then
                    return false
                end
            end
            local clean = string.format('git -C "%s" clean -fxd', path)
            if not system.exec(clean) then
                return false
            end
        end
    end
    return true
end

function RepoSource:update()
    local sync = string.format([[
sync -j %d --current-branch --no-tags --optimized-fetch]],
        self.jobs)
    return self:exec(sync)
end

function RepoSource:switch()
    -- Not doing Android development at the time.
    jagen.warning('branch switching is not implemented for repo sources')
    return true
end

function RepoSource:clone()
    local mkdir = { 'mkdir -p "'..self.dir..'"' }
    local init = { 'init', '-u', assert(self.location),
        '-b', assert(self.branch), '-p', 'linux', '--depth', 1
    }
    return system.exec(unpack(mkdir)) and self:exec(unpack(init)) and self:update()
end

return Source
