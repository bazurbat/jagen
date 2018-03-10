local Command = require 'Command'
local System = require 'System'
local Log    = require 'Log'

local Source = {}

function Source:new(o)
    local o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Source:is_known(tp)
    return tp == 'git' or tp == 'hg' or tp == 'repo'
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

function Source:getbranches()
    return { self.branch, unpack(self.branches or {}) }
end

function Source:gettags()
    return { self.tag, unpack(self.tags or {}) }
end

function Source:getbookmarks()
    return { self.bookmark, unpack(self.bookmarks or {}) }
end

function Source:getbranch()
    return self:getbranches()[1]
end

function Source:gettag()
    return self:gettags()[1]
end

function Source:getbookmark()
    return self:getbookmarks()[1]
end

function Source:getrev()
    return self:gettag() or self:getbookmark() or self:getbranch()
end

function Source:head()
end

function Source:fixup()
    return true
end

-- GitSource

function GitSource:command(...)
    return Command:new('git', '--no-pager', '-C', quote(assert(self.dir)), ...)
end

function GitSource:head()
    return self:command('rev-parse HEAD'):read()
end

function GitSource:dirty()
    return self:command('status --porcelain'):read() ~= nil
end

function GitSource:clean()
    return self:command('checkout HEAD -- .'):exec() and
           self:command('clean -fxd'):exec()
end

function GitSource:update_submodules()
    if not self.exclude_submodules and
            System.file_exists(System.mkpath(self.dir, '.gitmodules')) then
        return self:command('submodule update --init --recursive'):exec()
    end
    return true
end

function GitSource:update()
    local branch = self:getrev()
    if not branch then return false end
    local line = self:command('ls-remote --heads --tags origin', quote(branch)):read()
    if not line then
        line = self:command('branch --list', quote(branch)):read()
        if line then -- local branch, no update
            return true
        else
            Log.error("could not find tag or branch '%s' in '%s'", branch, self.location)
            return false
        end
    end
    local src, dst = string.match(line, '^%S+%s+(%S+)$')
    local name = assert(string.match(src, '^.+/([^/]+)$'))
    if string.match(src, '/tags/') then
        dst = string.format('refs/tags/%s', name)
    else
        dst = string.format('refs/remotes/origin/%s', name)
    end
    local refspec = string.format('+%s:%s', src, dst)
    return self:command('fetch --prune origin', quote(refspec)):exec() and
           self:update_submodules()
end

function GitSource:switch()
    local branch = self:getrev()
    if not branch then return true end

    local name = self:command('branch --list', quote(branch)):read()
    if name then
        if string.sub(name, 1, 1) == '*' then -- already active
            return true
        else -- switch to matching local branch
            name = string.trim(name)
            return self:command('checkout -q', quote(name)):exec() and
                   self:command('merge --ff-only', quote('origin/'..name)):read() and
                   self:update_submodules()
        end
    end

    name = self:command('branch --list --remotes', quote('origin/'..branch)):read()
    if name then -- switch to a new remote branch
        name = assert(string.match(name, '^%s+origin/(%S+)'))
        return self:command('checkout -b', quote(name), quote('origin/'..name)):exec() and
               self:update_submodules()
    end

    local tag = self:command('tag --list', quote(branch)):read()
    if tag then
        return self:command('checkout', quote(tag)):exec() and
               self:update_submodules()
    end

    Log.error("could not find tag or branch '%s' in '%s'", branch, self.dir)

    return false
end

function GitSource:clone()
    assert(self.location) assert(self.dir)
    local clone_cmd = Command:new('git clone --progress')
    for branch in each(self.branch or {}) do
        clone_cmd:append('--branch', branch)
    end
    -- try to detect if the server is "smart"
    -- https://stackoverflow.com/questions/9270488/is-it-possible-to-detect-whether-a-http-git-remote-is-smart-or-dumb
    local headers = Command:new('curl -sSi', quote(self.location..'/info/refs?service=git-upload-pack')):read('*a')
    if string.match(headers, 'Content%-Type: application/x%-git') then -- smart
        clone_cmd:append('--depth', 1)
    end
    clone_cmd:append('--', quote(self.location), quote(self.dir))
    return clone_cmd:exec() and self:update_submodules()
end

function GitSource:fixup()
    if self.assume_unchanged then
        return self:command('update-index --assume-unchanged',
                            quote(unpack(self.assume_unchanged)))
    end
    return true
end

-- HgSource

function HgSource:command(...)
    return Command:new('hg', '-y', '--pager', 'never', '-R', quote(assert(self.dir)), ...)
end

function HgSource:head()
    return self:command('id', '-i'):read()
end

function HgSource:dirty()
    return self:command('status'):read() ~= nil
end

function HgSource:clean()
    local purge_cmd = self:command('purge', '--all')
    local update_cmd = self:command('update', '--clean')
    local rev = self:getrev()
    if rev then update_cmd:append('--rev', rev) end
    return purge_cmd:exec() and update_cmd:exec()
end

function HgSource:update()
    local cmd = self:command('pull')
    for branch in each(self:getbranches()) do
        cmd:append('--branch', branch)
    end
    for bookmark in each(self:getbookmarks()) do
        cmd:append('--bookmark', bookmark)
    end
    for tag in each(self:gettags()) do
        cmd:append('--rev', tag)
    end
    return cmd:exec()
end

function HgSource:switch()
    local cmd = self:command('update', '--check')
    local rev = self:getrev()
    if rev then cmd:append('--rev', rev) end
    return cmd:exec()
end

function HgSource:clone()
    local cmd = Command:new('hg clone')
    for branch in each(self:getbranches()) do
        cmd:append('--branch', branch)
    end
    for rev in each(extend(self:getbookmarks(), self.gettags())) do
        cmd:append('--rev', rev)
    end
    cmd:append(quote(assert(self.location)))
    cmd:append(quote(assert(self.dir)))
    return cmd:exec()
end

-- RepoSource

function RepoSource:command(...)
    return Command:new('cd', quote(assert(self.dir)), '&&',
                       'repo', '--no-pager', ...)
end

function RepoSource:manifest_rev()
    local line = self:command('info', '-o'):read() or ''
    if line:match('^Manifest branch: ') then
        return assert(line:match('^.*/(.+)$') or line)
    else
        error(string.format('unexpected repo info format: %s', line))
    end
end

function RepoSource:reinit()
    local rev, manifest_rev = assert(self:getrev()), self:manifest_rev()
    if rev ~= manifest_rev then
        -- pipe to cat to inhibit prompting a user on a terminal
        return self:command('init', '-b', rev, '|', 'cat'):exec()
    end
    return true
end

function RepoSource:head()
    return self:manifest_rev()
end

function RepoSource:dirty()
    return not string.match(self:command('status'):read() or '', '^nothing')
end

function RepoSource:clean()
    local clean_cmd = 'if [ \"$(ls)\" ]; then git checkout HEAD -- . && git clean -fxd; fi'
    local sync_cmd = self:command('sync', '--detach', '--no-tags')
    if Jagen.flag 'offline' then
        sync_cmd:append('-l')
    end
    return sync_cmd:exec() and
           self:command('forall', '-pc', squote(clean_cmd)):exec()
end

function RepoSource:update()
    return self:reinit() and self:command('sync', '-nc'):exec()
end

function RepoSource:switch()
    return self:reinit() and self:command('sync', '-l'):exec()
end

function RepoSource:clone()
    return Command:new('mkdir', '-p', quote(assert(self.dir))):exec() and
           self:command('init', '-u', quote(assert(self.location)),
                                '-b', quote(assert(self:getrev()))
                                '--depth', 1):exec()
end

return Source
