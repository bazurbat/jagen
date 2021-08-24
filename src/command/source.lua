local Engine = require 'Engine'
local Options = require 'Options'
local Log = require 'Log'
local System = require 'System'

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
                error('could not find source packages matching: %s', pattern)
            end
        end
    else
        for _, pkg in pairs(packages) do
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

function P:update(args)
    local options = Options:new {
        { 'ignore-dirty,y' },
        { 'ignore-exclude,Y' }
    }
    args = options:parse(args)
    if not args then return false end

    local packages, ok = scm_packages(args), true
    local offline = Jagen.flag 'offline'
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

return P

