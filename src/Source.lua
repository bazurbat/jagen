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
        if #source == 1 then
            source.location = source[1]
            table.remove(source, 1)
        elseif #source == 2 then
            source.type = source[1]
            source.location = source[2]
            table.remove(source, 1)
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
        elseif url:match('%.tar$') or url:match('%.zip$') or
               url:match('%.tgz$') or url:match('%.gz$') or
               url:match('%.txz$') or url:match('%.xz$') or
               url:match('%.tbz2$') or url:match('%.bz2$')
        then
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
    local match
    for _, ext in ipairs { '%.tar%.%w+', '%.[^.]+' } do
        match = filename:match(string.format('^(.+)%s$', ext))
        if match then return match end
    end
    return filename
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
            source.filename = source.location:match('^.*/(.+)$') or source.location
        end
        if not source.basename then
            source.basename = source:_basename(source.filename)
        end
        if not source.dir then
            if source:is_scm() then
                source.dir = System.mkpath('$jagen_src_dir', source.basename)
            else
                source.dir = System.mkpath('$jagen_build_dir', name, source.basename)
            end
        end
    end

    if name and not source.dir then
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

function Source:getscmdir()
end

function Source:head()
    return ''
end

function Source:head_name()
    return self:head()
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

function GitSource:getscmdir()
    return System.mkpath(self.dir, '.git')
end

function GitSource:head()
    return self:command('rev-parse HEAD'):read()
end

function GitSource:head_name()
    local head = self:command('log -1 --format=\"%h%d\"'):read()
    if not head then return 'NONE' end
    local rev, ref = head:match('(%S+)%s%((.+)%)')
    rev = rev or head
    if ref then
        ref = ref:gsub('^HEAD%s+->%s+', '')
        ref = ref:gsub('^HEAD,%s+', '')
        ref = ref:gsub('^HEAD', '')
        ref = ref:gsub(self.origin..'/HEAD', '')
        ref = ref:gsub(', , ', ', ')
        ref = ref:gsub(', $', '')
        if #ref > 0 then
            return string.format('%s, %s', rev, ref)
        end
    end
    return rev
end

function GitSource:dirty()
    return self:command('status --porcelain'):read() ~= nil
end

function GitSource:clean()
    return self:command('checkout HEAD -- .'):exec() and
           self:command('clean -fxd'):exec()
end

function GitSource:update()
    local function extract_ref(line) return line:match('%w+%s+(.+)') end
    local function head_matching(name)
        return function (line) return line:match('^refs/heads/'..name:escape_pattern()..'$') end
    end
    local function tag_matching(name)
        return function (line) return line:match('^refs/tags/'..name:escape_pattern()..'$') end
    end
    if not self:command('remote set-url', assert(self.origin), quote(self.location)):exec() then
        return false
    end
    local refspecs, ok = {}, true
    local remotes = aslist(vmap(extract_ref)(self:command('ls-remote -q --heads --tags'):lines()))
    local rev, branches = self:getrev(), self:getbranches()
    if not rev then branches = { 'master' } end
    for branch in each(branches) do
        for ref in vfilter(head_matching(branch))(each(remotes)) do
            append(refspecs, string.format('"+refs/heads/%s:refs/remotes/%s/%s"',
                branch, self.origin, branch))
        end
    end
    for tag in each(self:gettags()) do
        for ref in vfilter(tag_matching(tag))(each(remotes)) do
            append(refspecs, string.format('"+refs/tags/%s:refs/tags/%s"', tag, tag))
        end
    end
    if self.rev then
        if #self.rev ~= 40 then
            Log.error('Git rev should be fully spelled (40 char) object name')
            return false
        end
        append(refspecs, string.format("%s", self.rev))
    end
    if #refspecs > 0 then
        return self:command('fetch', quote(self.origin), table.concat(refspecs, ' ')):exec() and
               self:_update_submodules()
    end
    return true
end

function GitSource:switch()
    local function checkout(ref)
        return self:command('checkout -q', quote(assert(ref)), '--'):exec()
    end
    local function update_submodules()
        return self:_update_submodules('--no-fetch')
    end
    local function verify(ref)
        return ref and self:command('show-ref -q --verify', ref):exec()
    end

    if self.rev then
        return checkout(self.rev) and update_submodules()
    end

    local tag = self:gettag()
    if tag then
        local ref = string.format('refs/tags/%s', tag)
        if verify(ref) then
            return checkout(ref) and update_submodules()
        else
            Log.error("the tag '%s' does not exist", tag)
            return false
        end
    end

    local branch = self:getbranch() or 'master'
    local ref = string.format('refs/heads/%s', branch)
    local remote_ref = string.format('refs/remotes/%s/%s', assert(self.origin), branch)
    local local_valid, remote_valid = verify(ref), verify(remote_ref)

    if local_valid then
        if not checkout(ref) then
            return false
        end
        if remote_valid then
            if not self:command('merge --ff-only', remote_ref):exec() then
                return false
            end
        end
    elseif remote_valid then
        if not checkout(remote_ref) then
            return false
        end
    else
        Log.error("the branch '%s' does not exist", branch)
        return false
    end

    return update_submodules()
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

function HgSource:getscmdir()
    return System.mkpath(self.dir, '.hg')
end

function HgSource:head()
    return self:command('id -i'):read()
end

function HgSource:head_name()
    local id = self:command('id -i'):read()
    local refs = self:command('id -nbB'):read()
    if id and refs then
        return string.format('%s, %s', id, refs)
    end
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

function HgSource:getscmdir()
    return System.mkpath(self.dir, '.repo')
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
