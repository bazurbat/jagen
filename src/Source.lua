local Command = require 'Command'
local System = require 'System'
local Log    = require 'Log'

local fmt = string.format

local Source = {}

function Source:new(o)
    local o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Source:parse(rule)
    if not rule then return end

    if type(rule) == 'string' then
        rule = { location = rule }
    elseif type(rule) == 'table' then
        if type(rule[1]) == 'string' then
            if type(rule[2]) == 'string' then
                rule.type = rule[1]
                rule.location = rule[2]
                table.remove(rule, 2)
            else
                rule.location = rule[1]
            end
            table.remove(rule, 1)
        end
    end

    local url = rule.location
    if not rule.type and rule.location then
        if url:match('%.gz$') or url:match('%.tgz$') or
           url:match('%.xz$') or url:match('%.txz$') or
           url:match('%.bz2$') or url:match('%.tbz2$') or
           url:match('%.zip$') or url:match('%.tar$') then
            rule.type = 'dist'
        elseif url:match('%.git$') or url:match('^git@') or
               url:match('^https?://github.com/') or
               url:match('^[%w._-]+@[%w._-]+:') then
            rule.type = 'git'
        elseif url:match('%.hg$') then
            rule.type = 'hg'
        elseif url == '.' then
            rule.type = 'dir'
            rule.location = System.expand('$jagen_project_dir')
        elseif not url:match('^.+://.+/.+$') and url:match('[%w_-]+') then
            rule.type = 'dist:gdrive'
        else
            rule.type = 'dist'
        end
    end

    return rule
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
        match = filename:match(fmt('^(.+)%s$', ext))
        if match then return match end
    end
    return filename
end

function Source:exists()
    return Command:newf([[
dir="%s" type="%s"; [ -d "$dir" -a -d "$dir/.$type" ] ]],
        self.dir, self.type):exec()
end

function Source:create(source, name)
    local source, dir = source or {}

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

function Source:getrev()
    return self.rev or self.bookmark or self.tag or self.branch
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
    local ok = true
    -- if not Jagen.flag 'offline' then
        ok = self:fetch()
    -- end
    return ok and self:switch()
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
    if o.origin == nil then
        o.origin = 'origin'
    end
    if o.shallow == nil then
        o.shallow = true
    end
    return o
end

function GitSource:_is_shallow()
    return System.file_exists(System.mkpath(assert(self.dir), '.git', 'shallow'))
end

function GitSource:_needs_submodules()
    if self.exclude_submodules then
        return false
    elseif self.__have_gitmodules ~= nil then
        return self.__have_gitmodules
    else
        self.__have_gitmodules = System.file_exists(System.mkpath(self.dir, '.gitmodules'))
        return self.__have_gitmodules
    end
end

function GitSource:_update_submodules(...)
    if self:_needs_submodules() then
        return self:command('submodule update --init --recursive', ...):exec()
    end
    return true
end

function GitSource:command(...)
    return Command:new('git', '--no-pager --git-dir=.git -C', quote(assert(self.dir)), ...)
end

function GitSource:getscmdir()
    return System.mkpath(self.dir, '.git')
end

function GitSource:head()
    return self:command('rev-parse HEAD 2>/dev/null'):read()
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
        if self.origin then
            ref = ref:gsub(self.origin..'/HEAD', '')
        end
        ref = ref:gsub(', , ', ', ')
        ref = ref:gsub(', $', '')
        if #ref > 0 then
            return fmt('%s, %s', rev, ref)
        end
    end
    return rev
end

function GitSource:dirty()
    return self:command('status --porcelain 2>/dev/null'):read() ~= nil
end

function GitSource:clean(ignored)
    local checkout = self:command('checkout HEAD -- .')
    local clean = self:command('clean -fd')
    if ignored then
        clean:append('-x')
    end
    return checkout:exec() and clean:exec()
end

function GitSource:_sync_config()
    local origin = self.origin
    if not origin then return true end

    if self.location then
        local url = self:command('config --local --get', quote('remote.'..origin..'.url')):read()
        if url and url ~= self.location then
            local cmd = self:command('remote set-url', quote(origin), quote(self.location))
            if not cmd:exec() then return false end
        end
    end

    local spec = fmt('+refs/heads/*:refs/remotes/%s/*', origin)
    if self.tag then
        spec = fmt('+refs/tags/%s:refs/tags/%s', self.tag, self.tag)
    elseif self.branch then
        spec = fmt('+refs/heads/%s:refs/remotes/%s/%s', self.branch, origin, self.branch)
    elseif self.shallow then
        local remote_head = self:command('remote show', quote(origin))
                                :match('^%s*HEAD branch: (%w+)$')
        local prefix = fmt('refs/remotes/%s/', origin)
        local local_head = self:command('symbolic-ref -q', quote(prefix..'HEAD'))
                               :match('^'..prefix..'(%w+)')
        local head = remote_head or local_head
        if head then
            spec = fmt('+refs/heads/%s:refs/remotes/%s/%s', head, origin, head)
        end
        self._fetch_remote_head = remote_head
        self._fetch_local_head = local_head
    end

    local key = fmt('remote.%s.fetch', origin)
    local val = self:command('config --local --get-all', quote(key)):read('*a'):trim()
    if val ~= spec then
        local cmd = self:command('config --replace-all', quote(key), quote(spec))
        if not cmd:exec() then return false end
    end

    return true
end

function GitSource:fetch()
    if not self:command('remote'):read() then -- local repo without remotes
        return true
    end

    if self.origin then
        local key = fmt('remote.%s.url', self.origin)
        local url = self:command('config --local --get', quote(key)):read()
        if not url then
            Log.error("could not find the specified remote '%s' in Git config of the package '%s'",
                self.origin, self.name)
            return false
        end
    end

    if not self:_sync_config() then return false end

    local fetch_cmd = self:command('fetch')
    if not self.shallow and self:_is_shallow() then
        fetch_cmd:append('--unshallow')
    end
    if self.origin then
        fetch_cmd:append(quote(self.origin))
    end

    local fetch_ok = fetch_cmd:exec()
    self._did_fetch = true

    if fetch_ok and self.origin then
        local rem, lcl = self._fetch_remote_head, self._fetch_local_head
        if rem and rem ~= lcl then
            self:command('remote set-head', quote(self.origin), '--auto'):exec()
        end
    end

    return fetch_ok and self:_update_submodules()
end

function GitSource:switch()
    local branch = self.branch
    if self.rev then
        branch = self.rev
    elseif self.tag then
        branch = self.tag
    end
    if branch then
        local cmd = self:command('checkout -q', quote(branch), '--')
        if not cmd:exec() then return false end
    end
    local function on_branch()
        return self:command('symbolic-ref -q HEAD'):read() ~= nil
    end
    if on_branch() then
        local remoteref
        if not branch and self._did_fetch then
            remoteref = 'FETCH_HEAD'
        elseif branch and self.origin then
            remoteref = fmt('refs/remotes/%s/%s', self.origin, branch)
        end
        if remoteref then
            local cmd
            if self.force_update then
                cmd = self:command('reset --hard', quote(remoteref), '--')
            else
                cmd = self:command('merge --ff-only', quote(remoteref))
            end
            if not cmd:exec() then return false end
        end
    end
    return self:_update_submodules('--no-fetch')
end

function GitSource:clone()
    assert(self.location) assert(self.dir)
    -- even for unattended cases the progress is useful to watch in logs
    local clone_cmd = Command:new('git', 'clone --progress')
    local branch = self.tag or self.branch
    if branch then
        clone_cmd:append('--branch', quote(branch))
    end
    if self.rev then
        -- the later checkout will fail if non-head revision is specified
        -- because shallow clone will not have it
        self.shallow = false
    end
    if self.shallow then
        local smart = false
        -- try to detect if the server is "smart"
        -- https://stackoverflow.com/questions/9270488/is-it-possible-to-detect-whether-a-http-git-remote-is-smart-or-dumb
        if self.location:match('^https?://') then
            local url = self.location..'/info/refs?service=git-upload-pack'
            local pattern = 'Content%-Type: application/x%-git'
            local command = Command:new('curl', '-fisS')
            if not string.empty(os.getenv('jagen_insecure')) then
                command:append('--insecure')
            end
            command:append(quote(url))
            if not command:exists() then
                Log.error("need 'curl' (command not found)")
                return false
            end
            if command:read('*a'):match(pattern) then
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
    return clone_cmd:exec() and self:update()
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
    -- The --pager=no option requires 'pager' extension to be enabled, we can
    -- set pager to nothing directly instead which always works.
    return Command:new('hg', '-y -R', quote(assert(self.dir)), '--config pager.pager=', ...)
end

function HgSource:getscmdir()
    return System.mkpath(assert(self.dir), '.hg')
end

function HgSource:head()
    return self:command('id -i 2>/dev/null'):read()
end

function HgSource:head_name()
    local id = self:command('id -i'):read()
    local refs = self:command('id -nbB'):read()
    if id and refs then
        return fmt('%s, %s', id, refs)
    end
end

function HgSource:dirty()
    return self:command('status 2>/dev/null'):read() ~= nil
end

function HgSource:clean(ignored)
    local update = self:command('update --clean')
    local rev = self:getrev()
    if rev then update:append('--rev', quote(rev)) end
    local purge  = self:command('--config', 'extensions.purge=', 'purge')
    if ignored then purge:append('--all') end
    return update:exec() and purge:exec()
end

function HgSource:fetch()
    local cmd = self:command('pull')
    if self.rev then
        cmd:append('--rev', quote(self.rev))
    elseif self.bookmark or self.tag then
        cmd:append('--bookmark', quote(self.bookmark or self.tag))
    elseif self.branch then
        cmd:append('--branch', quote(self.branch))
    end
    return cmd:exec()
end

function HgSource:switch()
    local cmd = self:command('update --check')
    local rev = self:getrev()
    if rev then cmd:append('--rev', quote(rev)) end
    return cmd:exec()
end

function HgSource:clone()
    assert(self.location) assert(self.dir)
    local cmd = Command:new('hg', 'clone')
    local rev = self.rev or self.bookmark or self.tag
    if rev then
        cmd:append('--rev', quote(rev))
    elseif self.branch then
        cmd:append('--branch', quote(self.branch))
    end
    cmd:append(quote(self.location))
    cmd:append(quote(self.dir))
    return cmd:exec()
end

-- RepoSource

function RepoSource:command(...)
    return Command:new('cd', quote(assert(self.dir)), '&&',
                       'repo', '--no-pager', ...)
end

function RepoSource:getscmdir()
    return System.mkpath(self.dir, '.repo')
end

function RepoSource:manifest_rev()
    local line = self:command('info -o'):read() or ''
    if line:match('^Manifest branch: ') then
        return assert(line:match('^.*/(.+)$') or line)
    else
        error(fmt('unexpected repo info format: %s', line))
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
    -- if Jagen.flag 'offline' then
        -- sync_cmd:append('-l')
    -- end
    return sync_cmd:exec() and
           self:command('forall -pc', squote(clean_cmd)):exec()
end

function RepoSource:fetch()
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
