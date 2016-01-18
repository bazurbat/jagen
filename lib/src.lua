--{{{ Source

Source = {}

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

function Source:create(source)
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
        local dir = source:is_scm() and '$jagen_src_dir' or '$pkg_work_dir'
        local basename = source:basename(source.location)
        source.path = system.mkpath(dir, source.path or basename)
    end

    return source
end

function Source:new(o)
    local o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

GitSource = Source:new()

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

function GitSource:fetch(branch)
    local cmd = { 'fetch', '--prune', '--no-tags', 'origin' }
    if branch then
        local src = 'refs/heads/'..branch
        local dst = 'refs/remotes/origin/'..branch
        table.insert(cmd, '+'..src..':'..dst)
    end
    return self:exec(unpack(cmd))
end

function GitSource:checkout(branch)
    assert(branch)
    local name = self:popen('branch', '--list', branch)
    if name and #name > 0 then
        if string.sub(name, 1, 1) == '*' then
            return true
        else
            return self:exec('checkout', branch)
        end
    else
        local add = { 'remote', 'set-branches', '--add', 'origin', branch }
        local checkout = { 'checkout', '-b', branch, '-t', 'origin/'..branch }
        return self:exec(unpack(add)) and self:exec(unpack(checkout))
    end
end

function GitSource:merge(branch)
    return self:exec('merge', '--ff-only', 'origin/'..assert(branch))
end

function GitSource:update()
    local branch = assert(self.branch)
    return self:fetch(branch) and self:checkout(branch) and self:merge(branch)
end

function GitSource:clone()
    return system.exec('git', 'clone', '--branch', assert(self.branch),
        '--depth', 1, assert(self.location), assert(self.path))
end

HgSource = Source:new()

function HgSource:new(o)
    local source = Source.new(HgSource, o)
    source.branch = source.branch or 'default'
    return source
end

function HgSource:exec(...)
    return system.exec('hg', '-R', assert(self.path), ...)
end

function HgSource:popen(...)
    return system.popen('hg', '-R', assert(self.path), ...):read()
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
    local pull = { 'pull', '-r', assert(self.branch) }
    local update = { 'update', '-r', assert(self.branch) }
    return self:exec(unpack(pull)) and self:exec(unpack(update))
end

function HgSource:clone()
    return system.exec('hg', 'clone', '-r', assert(self.branch),
        assert(self.location), assert(self.path))
end

RepoSource = Source:new()

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

function RepoSource:load_projects(...)
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

function RepoSource:head()
    return self:popen('status', '-j', 1, '--orphans'):read('*all')
end

function RepoSource:dirty()
    return false
end

function RepoSource:clean()
    local projects = self:load_projects()
    local function is_empty(path)
        return system.popen('cd', '"'..path..'"', '&&', 'echo', '*'):read() == '*'
    end
    for n, p in pairs(projects) do
        local path = system.mkpath(self.path, p)
        if not is_empty(path) then
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

function RepoSource:clone()
    local mkdir = { 'mkdir -p "'..self.path..'"' }
    local init = { 'init', '-u', assert(self.location),
        '-b', assert(self.branch), '-p', 'linux', '--depth', 1
    }
    return system.exec(unpack(mkdir)) and self:exec(unpack(init)) and self:update()
end

--}}}

--{{{ SourceManager

SourceManager = {}

function SourceManager:new(o)
    local o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function SourceManager:packages(names)
    local packages, scm_packages = Rules.load(), {}
    if names and #names > 0 then
        for _, name in ipairs(names) do
            if not packages[name] then
                jagen.die('no such package: %s', name)
            end
            if not packages[name].source:is_scm() then
                jagen.die('not scm package: %s', name)
            end
            table.insert(scm_packages, packages[name])
        end
    else
        for _, pkg in ipairs(packages) do
            if pkg.source:is_scm() then
                table.insert(scm_packages, pkg)
            end
        end
    end
    return scm_packages
end

-- Should return 0 if true, 1 if false, for shell scripting.
function SourceManager:dirty_command(names)
    for _, pkg in ipairs(self:packages(names)) do
        if pkg.source:dirty() then
            return 0
        end
    end
    return 1
end

function SourceManager:status_command(names)
    for _, pkg in ipairs(self:packages(names)) do
        local source = pkg.source
        if system.exists(source.path) then
            local dirty = source:dirty() and 'dirty' or ''
            local head = source:head()
            if not head then
                jagen.die('failed to get source head for %s in %s',
                    pkg.name, source.path)
            end
            print(string.format("%s (%s): %s %s", pkg.name, source.location, head, dirty))
        else
            print(string.format("%s (%s): not exists", pkg.name, source.location))
        end
    end
end

function SourceManager:clean_command(names)
    for _, pkg in ipairs(self:packages(names)) do
        if not pkg.source:clean() then
            jagen.die('failed to clean %s (%s) in %s',
                pkg.name, pkg.source.branch, pkg.source.path)
        end
    end
end

function SourceManager:update_command(names)
    for _, pkg in ipairs(self:packages(names)) do
        if not pkg.source:update() then
            jagen.die('failed to update %s to the latest %s in %s',
                pkg.name, pkg.source.branch, pkg.source.path)
        end
    end
end

function SourceManager:clone_command(names)
    for _, pkg in ipairs(self:packages(names)) do
        if not pkg.source:clone() then
            jagen.die('failed to clone %s from %s to %s',
                pkg.name, pkg.source.location, pkg.source.path)
        end
    end
end

function SourceManager:delete_command(names)
    for _, pkg in ipairs(self:packages(names)) do
        if system.exists(pkg.source.path) then
            if not system.exec('rm', '-rf', pkg.source.path) then
                jagen.die('failed to delete %s source directory %s',
                    pkg.name, pkg.source.path)
            end
        end
    end
end

--}}}
