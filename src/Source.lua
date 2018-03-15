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

function Source:parse(rule)
    local source

    if type(rule) == 'table' then
        source = rule
        if type(source[1]) == 'string' then
            if source.location == nil then
                source.location = source[1]
            end
            table.remove(source, 1)
        end
    elseif type(rule) == 'string' then
        source = { location = rule }
    end

    if source and not source.type and source.location then
        local url = source.location
        if url:match('%.hg$') then
            source.type = 'hg'
        elseif url:match('%.git$') or url:match('^git@') or
               url:match('^[%w._-]+@[%w._-]+:') then
            source.type = 'git'
        else
            source.type = 'dist'
        end
    end

    return source
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

function Source:clean_disabled()
    if self.exclude then
        return 'excluded'
    elseif self:dirty() then
        return 'dirty'
    else
        return false
    end
end

function Source:getbranches()
    return prepend(self.branches or {}, self.branch)
end

function Source:gettags()
    return prepend(self.tags or {}, self.tag)
end

function Source:getbookmarks()
    return prepend(self.bookmarks or {}, self.bookmark)
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
    return self.rev or self:gettag() or self:getbookmark() or self:getbranch()
end

function Source:head()
    return ''
end

function Source:dirty()
    return false
end

function Source:clean()
    return true
end

function Source:update()
    return true
end

function Source:switch()
    return true
end

function Source:clone()
    return true
end

function Source:fixup()
    return true
end

-- GitSource

function GitSource:new(o)
    local o = Source.new(self, o)
    o.origin = o.origin or 'origin'
    return o
end

function GitSource:_needs_submodules()
    return not self.exclude_submodules and
        System.file_exists(System.mkpath(assert(self.dir), '.gitmodules'))
end

function GitSource:_update_submodules(...)
    if self:_needs_submodules() then
        return self:command('submodule update --init --recursive', ...):exec()
    end
    return true
end

function GitSource:command(...)
    return Command:new('git --no-pager --git-dir=.git -C', quote(assert(self.dir)), ...)
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

function GitSource:update()
    assert(self.origin)
    local refspecs = {}
    for branch in each(self:getbranches()) do
        append(refspecs, string.format('"+refs/heads/%s:refs/remotes/%s/%s"',
            branch, self.origin, branch))
    end
    for tag in each(self:gettags()) do
        append(refspecs, string.format('"+refs/tags/%s:refs/tags/%s"',
            tag, tag))
    end
    if self.rev then
        append(refspecs, string.format("%s", self.rev))
    end
    return self:command('fetch', quote(self.origin),
               table.concat(refspecs, ' ')):exec() and
           self:_update_submodules()
end

function GitSource:switch()
    local tag, branch, ref = self:gettag(), self:getbranch() or 'master'
    if self.rev then
        ref = self.rev
    elseif tag then
        ref = string.format('tags/%s', tag)
    elseif branch then
        ref = string.format('%s/%s', assert(self.origin), branch)
    end
    if ref then
        return self:command('checkout -q', quote(ref), '--'):exec() and
               self:_update_submodules('--no-fetch')
    end
end

function GitSource:clone()
    assert(self.location) assert(self.dir)
    -- even for unattended cases the progress is useful to watch in logs
    local clone_cmd = Command:new('git clone --progress')
    for branch in each(self:getbranches()) do
        clone_cmd:append('--branch', quote(branch))
    end
    if self.shallow then
        local smart = false
        -- try to detect if the server is "smart"
        -- https://stackoverflow.com/questions/9270488/is-it-possible-to-detect-whether-a-http-git-remote-is-smart-or-dumb
        if self.location:match('^https?://') then
            local url = self.location..'/info/refs?service=git-upload-pack'
            local pattern = 'Content%-Type: application/x%-git'
            local command = Command:new('curl -fisS', quote(url))
            if command:exists() and command:read('*a'):match(pattern) then
                smart = true
            end
        else
            smart = true
        end
        if smart then
            clone_cmd:append('--depth', 1)
        end
    end
    clone_cmd:append('--', quote(self.location), quote(self.dir))
    return clone_cmd:exec() and self:_update_submodules()
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
    return Command:new('hg -y --pager never -R', quote(assert(self.dir)), ...)
end

function HgSource:head()
    return self:command('id -i'):read()
end

function HgSource:dirty()
    return self:command('status'):read() ~= nil
end

function HgSource:clean()
    local purge_cmd = self:command('purge --all')
    local update_cmd = self:command('update --clean')
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
    for tag in each(append(self:gettags(), self.rev)) do
        cmd:append('--rev', tag)
    end
    return cmd:exec()
end

function HgSource:switch()
    local cmd = self:command('update --check')
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
    local line = self:command('info -o'):read() or ''
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
        return self:command('init -b', quote(rev), '| cat'):exec()
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
    local sync_cmd = self:command('sync --detach --no-tags')
    if Jagen.flag 'offline' then
        sync_cmd:append('-l')
    end
    return sync_cmd:exec() and
           self:command('forall -pc', squote(clean_cmd)):exec()
end

function RepoSource:update()
    return self:reinit() and self:command('sync -nc'):exec()
end

function RepoSource:switch()
    return self:reinit() and self:command('sync -l'):exec()
end

function RepoSource:clone()
    -- repo sources are used for Android mostly, which is huge, always specify
    -- depth to save disk space and let the Repo tool deal with it
    return Command:new('mkdir', '-p', quote(assert(self.dir))):exec() and
           self:command('init', '-u', quote(assert(self.location)),
                                '-b', quote(assert(self:getrev()))
                                '--depth', 1):exec()
end

return Source
