local Engine  = require 'Engine'
local Options = require 'Options'
local Log     = require 'Log'
local System  = require 'System'
local Target  = require 'Target'

local P = {}

local function scm_packages(patterns)
    local engine = Engine:new()
    local packages = engine:load_rules()
    local o = {}

    if patterns and #patterns > 0 then
        for pattern in each(patterns) do
            local cpattern, found = string.convert_pattern(pattern), false
            for name, pkg in kvpairs(packages) do
                if name:match(cpattern) and pkg.source.scm then
                    table.insert(o, pkg) found = true
                end
            end
            if not found then
                error(string.format('could not find source packages matching: %s', pattern))
            end
        end
    else
        for _, pkg in kvpairs(packages) do
            if pkg.source and pkg.source.scm then
                table.insert(o, pkg)
            end
        end
    end

    table.sort(o, function (a, b)
            return a.name < b.name
        end)

    return o
end

function P:dirty(args)
    local packages = scm_packages(args)
    for _, pkg in ipairs(packages) do
        local source = pkg.source
        if System.exists(source.dir) and not System.is_empty(source.dir) and
                source:dirty() then
            return true
        end
    end
    return false
end

function P:status(args)
    local packages = scm_packages(args)
    for _, pkg in ipairs(packages) do
        local source = pkg.source
        if System.exists(source.dir) and System.exists(source:getscmdir()) then
            local head = source:head_name()
            local dirty = source:dirty() and ' dirty' or ''
            if #dirty > 0 and source.ignore_dirty then
                dirty = string.format(' dirty(ignored:%s)', tostring(source.ignore_dirty))
            end
            local exclude = source.exclude and ' excluded' or ''
            print(string.format("%s%s%s%s%s", pkg.name,
                    source.location and ' ('..source.location..')',
                    head and ' ['..head..']', dirty, exclude))
        else
            print(string.format("%s (%s): not cloned", pkg.name, source.location))
        end
    end
    return true
end

function P:update(args)
    local options = Options:new {
        { 'ignore-dirty,y' },
        { 'ignore-exclude,Y' }
    }
    args = options:parse(args)
    if not args then return false end

    local packages, ok = scm_packages(args), true
    local offline = false -- Jagen.flag 'offline'
    local force_exclude = os.getenv('jagen__force_exclude')
    local ignore_dirty = args['ignore-dirty'] or os.getenv('jagen__ignore_dirty')
    local ignore_exclude = args['ignore-exclude'] or os.getenv('jagen__ignore_exclude')

    function update(source)
        local old_head = source:head()
        local ok = source:update()
        if ok and source:head() ~= old_head then
            assert(Target.from_args(assert(source.name), 'unpack'):touch())
        end
        return ok
    end

    -- Sorting from the shortest to the longest is needed for the case when the
    -- source directories are specified inside each other and we need to clone
    -- both: deeper one is cloned first, then clone complains about already
    -- existing non-empty directory.
    table.sort(packages, function (a, b)
            return (a.source.dir or '') < (b.source.dir or '')
        end)

    for pkg in each(packages) do
        local source = pkg.source
        local dir = System.expand(source.dir)
        if not source:exists() then
            if offline then
                Log.warning("not cloning %s: offline mode", pkg.name)
                ok = false
            else
                if not source.location then
                    Log.message("not cloning %s: the source location is not specified", pkg.name)
                else
                    Log.message('cloning %s from %s', pkg.name, source.location)
                    if not source:clone() then
                        Log.warning('failed to clone %s from %s to %s', pkg.name, source.location, dir)
                        ok = false
                    end
                end
            end
        else
            if force_exclude or not ignore_exclude and source.exclude then
                Log.message("not updating %s: the source is excluded", pkg.name)
            elseif source:dirty() then
                ignore_dirty = ignore_dirty and 'forced' or source.ignore_dirty
                if ignore_dirty then
                    Log.message("updating %s: ignoring dirty status of '%s' (%s)", pkg.name, dir, tostring(ignore_dirty))
                    ok = update(source)
                else
                    Log.warning("not updating %s: the source directory '%s' has unsaved changes", pkg.name, dir)
                    ok = false
                end
            else
                Log.message("updating %s: %s", pkg.name, dir)
                ok = update(source)
            end
        end

        if source:exists() then
            if not source:fixup() then
                Log.warning('failed to fix up %s in %s', pkg.name, dir)
                ok = false
            end
        end
    end

    return ok
end

function P:clean(args)
    local options = Options:new {
        { 'ignored,i' },
        { 'ignore-dirty,y' },
        { 'ignore-exclude,Y' }
    }
    args = options:parse(args)
    if not args then return false end

    local packages, ok   = scm_packages(args), true
    local force_exclude  = os.getenv('jagen__force_exclude')
    local clean_ignored  = args['ignored']        or os.getenv('jagen__clean_ignored')
    local ignore_dirty   = args['ignore-dirty']   or os.getenv('jagen__ignore_dirty')
    local ignore_exclude = args['ignore-exclude'] or os.getenv('jagen__ignore_exclude')

    for pkg in each(packages) do
        local source = pkg.source
        local dir = System.expand(source.dir)
        local willclean, reason, comment
        if force_exclude or not ignore_exclude and source.exclude then
            reason = "the source is excluded"
            if force_exclude then
                comment = 'forced by the command argument'
            end
        elseif source:exists() then
            if source:dirty() then
                if ignore_dirty then
                    comment = 'forced by the command argument'
                end
                ignore_dirty = ignore_dirty or source.ignore_dirty
                if ignore_dirty then
                    willclean = true
                    reason = 'a dirty status is ignored'
                    if type(ignore_dirty) == 'string' then
                        comment = ignore_dirty
                    end
                else
                    reason = "the source directory has unsaved changes (dirty)"
                end
            else
                willclean = true
                if source.exclude then
                    reason = 'an exclude is ignored'
                    if ignore_exclude then
                        comment = 'forced by the command argument'
                    end
                end
            end
        else
            reason = 'the source directory does not exist'
        end

        if not source.exclude and not force_exclude then
            clean_ignored = true
        end

        local message = 'cleaning'
        if willclean then
            if clean_ignored then
                message = 'completely '..message
            end
        else
            message = 'not '..message
        end
        message = message..' '..dir
        if reason then
            message = message..': '..reason
        end
        if comment then
            message = message..' ('..comment..')'
        end

        Log.message(message)

        if willclean then
            if source:clean(clean_ignored) then
                assert(Target.from_args(assert(source.name), 'clean'):touch())
            else
                ok = false
            end
        end
    end

    return ok
end

function P:delete(args)
    local packages = scm_packages(args)
    for _, pkg in ipairs(packages) do
        local source = pkg.source
        if System.exists(source.dir) then
            if not System.rmrf(source.dir) then
                die('failed to delete %s source directory %s',
                    pkg.name, source.dir)
            end
        end
    end
end

function P:each(args)
    local options = Options:new {
        { 'help,h' },
        { 'type=' },
    }
    args = options:parse(args)
    if not args then
        return 22
    end
    if args['help'] then
        return Jagen.command['help'] { 'src_each' }
    end
    local packages = scm_packages()
    for pkg in each(packages) do
        local arg_type, src_type = args['type'], pkg.source.type
        if arg_type and not Source:is_known(arg_type) then
            die('unknown source type: %s', arg_type)
        end
        if #args < 1 then
            die('the command is not specified')
        end
        local cmd = table.concat(args, ' ')
        if not arg_type then
            local bin = string.match(cmd, '^(%w+)')
            if Source:is_known(bin) then
                arg_type = bin
            end
        end
        local dir = System.expand(pkg.source.dir)
        local cmd = string.format('cd "%s" && %s', dir, cmd)
        if not arg_type or arg_type == src_type then
            Log.message('%s: %s', pkg.name, dir)
            local ok, status = Command:new(cmd):exec()
            if not ok then return status end
        end
    end
end

return P
