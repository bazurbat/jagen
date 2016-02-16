
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
        source.path = system.mkpath(dir, source.path or basename)
    end

    return source
end

-- GitSource

function GitSource:new(o)
    local source = Source.new(GitSource, o)
    source.branch = source.branch or 'master'
    return source
end

function GitSource:exec(...)
    return system.exec('git', '-C', assert(self.path), ...)
end

function GitSource:popen(...)
    return system.popen('git', '-C', assert(self.path), ...):read()
end

function GitSource:head()
    return self:popen('rev-parse', 'HEAD')
end

function GitSource:dirty()
    return self:popen('status', '--porcelain')
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
    local branch = self:popen('branch', '-a', '--list', pattern)
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
        '--depth', 1, assert(self.location), assert(self.path))
end

-- HgSource

function HgSource:new(o)
    local source = Source.new(HgSource, o)
    source.branch = source.branch or 'default'
    return source
end

function HgSource:exec(...)
    return system.exec('hg', '-R', assert(self.path), ...)
end

function HgSource:popen(...)
    return system.popen('hg', '-R', assert(self.path), ...):read('*a')
end

function HgSource:head()
    return self:popen('id', '-i')
end

function HgSource:dirty()
    return self:popen('status')
end

function HgSource:clean()
    return self:exec('update', '-C', assert(self.branch)) and
           self:exec('purge', '--all')
end

function HgSource:update()
    local cmd = { 'pull', '-r', assert(self.branch) }
    return self:exec(unpack(cmd))
end

function HgSource:_is_bookmark(pattern)
    local bm = self:popen("bookmarks | grep '^..."..pattern.."\\s'")
    local exists, active = false, false

    if bm and #bm > 0 then
        exists = true
        active = string.sub(bm, 2, 2) == '*'
    end

    return exists, active
end

function HgSource:switch()
    local branch = assert(self.branch)
    local exists, active = self:_is_bookmark(branch)
    if active then
        return true
    elseif exists then
        local cmd = { 'update', '-r', assert(self.branch) }
        return self:exec(unpack(cmd))
    else
        jagen.error("could not find bookmark '%s' in local repository", branch)
        return false
    end
end

function HgSource:clone()
    return system.exec('hg', 'clone', '-r', assert(self.branch),
        assert(self.location), assert(self.path))
end

-- RepoSource

function RepoSource:new(o)
    local source = Source.new(RepoSource, o)
    source.jobs = jagen.nproc * 2
    return source
end

function RepoSource:exec(...)
    local cmd = { 'cd', '"'..assert(self.path)..'"', '&&', 'repo', ... }
    return system.exec(unpack(cmd))
end

function RepoSource:popen(...)
    local cmd = { 'cd', '"'..assert(self.path)..'"', '&&', 'repo', ... }
    return system.popen(unpack(cmd))
end

function RepoSource:head()
    return self:popen('status', '-j', 1, '--orphans'):read('*all')
end

function RepoSource:dirty()
    return false
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

function RepoSource:clean()
    local projects = self:_load_projects()
    local function is_empty(path)
        return system.popen('cd', '"'..path..'"', '&&', 'echo', '*'):read() == '*'
    end
    local function is_dirty(path)
        local cmd = { 'git', '-C', assert(path), 'status', '--porcelain' }
        return system.popen(unpack(cmd)):read() ~= nil
    end
    for n, p in pairs(projects) do
        local path = system.mkpath(self.path, p)
        if not is_empty(path) and is_dirty(path) then
            if not system.exec('git', '-C', path, 'checkout', 'HEAD', '.') then
                return false
            end
            if not system.exec('git', '-C', path, 'clean', '-fxd') then
                return false
            end
        end
    end
    return true
end

function RepoSource:update()
    local cmd = { 'sync', '-j', self.jobs, '--current-branch', '--no-tags',
        '--optimized-fetch'
    }
    return self:exec(unpack(cmd))
end

function RepoSource:switch()
    return true
    -- TODO
    -- local cmd = { 'checkout', assert(self.branch) }
    -- return self:exec(unpack(cmd))
end

function RepoSource:clone()
    local mkdir = { 'mkdir -p "'..self.path..'"' }
    local init = { 'init', '-u', assert(self.location),
        '-b', assert(self.branch), '-p', 'linux', '--depth', 1
    }
    return system.exec(unpack(mkdir)) and self:exec(unpack(init)) and self:update()
end

return Source
