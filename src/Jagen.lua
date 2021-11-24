require 'common'

local System  = require 'System'
local Engine   = require 'Engine'
local Target  = require 'Target'
local Source  = require 'Source'
local Log     = require 'Log'
local Ninja   = require 'Ninja'
local Options = require 'Options'
local Command = require 'Command'

local function die(...)
    Log.error(...)
    os.exit(1)
end

Jagen = {}
Jagen.command = {}

local function help_requested(args)
    return args and args[1] and
        (args[1] == '-h' or string.match(args[1], '^%--help$'))
end

function Jagen.command.help(args, short)
    topic = args[1] or 'usage'
    local help = require 'help'
    local section = help[topic]
    if not section then
        Log.error('no such help topic: %s', topic)
        return 2
    elseif type(section) == 'table' then
        if section['usage'] then
            io.write(section['usage'])
        end
        if section['synopsis'] then
            if short then
                io.write(string.format([[
  Use the command 'jagen help %s' or '--help' argument for more information.

]], topic))
            else
                io.write(section['synopsis'])
            end
        end
    elseif type(section) == 'string' then
        io.write(section)
    end
end

function Jagen.command.clean(args)
    local options = Options:new {
        { 'help,h' },
        { 'match,m' },
        { 'ignored,i' },
        { 'exclude,x' },
        { 'ignore-dirty,y' },
        { 'ignore-exclude,Y' }
    }
    args = options:parse(args)
    if not args then return false end

    if args['help'] then
        return Jagen.command['help'] { 'clean' }
    end

    local force_exclude = args['exclude'] or os.getenv('jagen__force_exclude')
    local clean_ignored = args['ignored'] or os.getenv('jagen__clean_ignored')
    local ignore_dirty = args['ignore-dirty'] or os.getenv('jagen__ignore_dirty')
    local ignore_exclude = args['ignore-exclude'] or os.getenv('jagen__ignore_exclude')

    local packages = Engine:load()

    -- if not Engine:validate() then
    --     Log.error('aborting clean due to rule errors')
    --     return false
    -- end

    local clean_all = false
    if #args == 0 then
        clean_all = true
        append(args, '*')
    end

    local specs = {}
    local function add_matching(pkg, pattern)
        local target = pkg:find_target(pattern)
        if target then
            append(specs, tostring(target))
            return true
        end
    end
    for arg in each(args) do
        local split, pname, pconf = arg:split(':', 1)
        pname = string.to_line_pattern(split[1])
        if split[2] then
            pconf = string.to_stage_pattern(split[2])..'$'
        end
        for name, pkg in iter(packages, filter(function (_, name) return name:match(pname) end)) do
            if not pconf then
                found = add_matching(pkg, string.format('%s:clean', name))
            end
            found = add_matching(pkg, string.format('%s:clean:%s', name, pconf or string.to_stage_pattern('*'))) or found
        end
        if not found and not (clean_all and arg == '*') then
            Log.warning('could not find targets matching: %s', arg)
        end
    end

    if args['match'] then
        for spec in each(specs) do
            print(spec)
        end
        return true
    end

    if clean_all then
        local clean_dirs = {
            'jagen_bin_dir',
            'jagen_build_dir',
            'jagen_include_dir',
            'jagen_log_dir',
        }
        if not System.rmrf(unpack(System.getenv(clean_dirs))) then
            return false
        end
        if not Jagen.command.refresh() then
            return false
        end
    else
        if not found then return false end
    end

    append(specs, '--quiet', '--no-auto')
    if force_exclude then
        append(specs, '--exclude')
    end
    if clean_ignored then
        append(specs, '--clean-ignored')
    end
    if ignore_dirty then
        append(specs, '--ignore-dirty')
    end
    if ignore_exclude then
        append(specs, '--ignore-exclude')
    end

    return Jagen.command.build(specs)
end

function Jagen.command.refresh(args)
    if help_requested(args) then
        return Jagen.command['help'] { 'refresh' }
    end

    return require('Refresh'):run(args)
end

function Jagen.command.build(args)
    if help_requested(args) then
        return Jagen.command['help'] { 'refresh' }
    end

    return require('command/build'):run(args)
end

Jagen.command['rebuild'] = Jagen.command.build
Jagen.command['re'] = Jagen.command.build
Jagen.command['do'] = Jagen.command.build

function Jagen.command.source(args)
    if help_requested(args) then
        return Jagen.command.help { 'source' }
    end
    local source = require('command/source')
    local command = args[1]
    local func = source[command]
    if func then
        table.remove(args, 1)
        return func(source, args)
    else
        die('invalid source subcommand: %s', command)
    end
end
Jagen.command.src = Jagen.command.source

function Jagen.command.list(args)
    if #args == 0 or help_requested(args) then
        return Jagen.command['help'] { 'list' }
    end

    if args[1] ~= 'packages' and args[1] ~= 'layers' then
        die("invalid list command '"..args[1].."', try 'jagen list help'")
    end

    if args[1] == 'layers' then
        local layers = Jagen._load_layers()
        for name, path in pairs(layers) do
            io.write(string.format('%s: %s\n', name, path))
        end
        return true
    end

    local options = Options:new {
        { 'help,h' },
        { 'depth,d=n', 0, 999 },
        { 'all,a', false }
    }
    args = options:parse(table.rest(args, 2))
    if not args then
        return 22
    end

    if args['help'] then
        return Jagen.command['help'] { 'list' }
    end

    local depth, show_all = args['depth'], args['all']

    local packages = Engine:load()
    local pkg_list, name_max = {}, 0
    for name, pkg in pairs(packages) do
        if #name > name_max then name_max = #name end
        table.insert(pkg_list, pkg)
    end
    table.sort(pkg_list, function (a, b) return a.name < b.name end)
    local start_col = name_max + 2

    if not show_all then
        -- prune implicit contexts
        for _, pkg in pairs(packages) do
            local contexts = {}
            for context in each(pkg.contexts) do
                if not context.implicit then
                    table.insert(contexts, context)
                end
            end
            pkg.contexts = contexts
        end
    end

    -- prune up to the specified depth
    for _, pkg in pairs(packages) do
        pkg.contexts = copy(pkg.contexts)
        for _, context in ipairs(pkg.contexts) do
            local level = 0
            while context do
                if level >= depth then
                    context.parent = nil
                end
                context = context.parent
                level = level + 1
            end
        end
    end

    -- prune duplicate entries left after pruning
    for name, pkg in pairs(packages) do
        local tt, tag = {}
        for _, c in ipairs(pkg.contexts) do
            tag = -c
            if not tt[tag] then
                tt[tag] = c
                table.insert(tt, c)
            end
        end
        pkg.contexts = {}
        for _, c in ipairs(tt) do
            table.insert(pkg.contexts, c)
        end
    end

    for _, pkg in ipairs(pkg_list) do
        io.write(pkg.name, Engine:format_contexts(pkg, start_col, start_col - #pkg.name), '\n')
    end

    return true
end

function Jagen.command.update(args)
    if help_requested(args) then
        return Jagen.command['help'] { 'update' }
    end

    local options = Options:new { { 'help,h' } }
    args = options:parse(table.rest(args, 1))
    if not args then return 22 end

    if args['help'] then
        return Jagen.command['help'] { 'update' }
    end

    if #args == 0 then
        table.insert(args, '*')
    end

    local layers, sources = Jagen._load_layers(), {}
    for name, path in pairs(layers) do
        sources[name] = Source:create { type = 'git', dir = path }
    end

    local keys = {}
    for arg in each(args) do
        if arg == 'self' or arg == 'jagen' then
            table.insert(keys, 'jagen') matched = true
        else
            local pattern, matched = string.convert_pattern(arg), false
            for name, _ in pairs(sources) do
                if name:match(pattern) then
                    table.insert(keys, name) matched = true
                end
            end
            if not matched then
                die("Could not find any layer matching: %s", arg)
            end
        end
    end

    sources['jagen'] = Source:create { type = 'git', dir = '$jagen_dir' }

    local retval = true
    for key in each(keys) do
        local source = sources[key]
        if source:dirty() then
            Log.warning("not updating %s: the source directory '%s' has unsaved changes", key, System.expand(source.dir))
            retval = false
        else
            Log.message("updating %s", key)
            if not source:fetch() or not source:switch() then
                Log.warning("failed to update %s in %s", key, System.expand(source.dir))
                retval = false
            end
        end
    end

    return retval
end

function Jagen.command._compare_versions(args)
    assert(#args == 3, "function 'compare_versions' requires 3 arguments")
    local op_arg = assert(args[1])
    local va_arg = assert(args[2])
    local vb_arg = assert(args[3])

    local ops = {
        ['eq'] = function(a, b) return a == b end,
        ['ne'] = function(a, b) return a ~= b end,
        ['gt'] = function(a, b) return a >  b end,
        ['ge'] = function(a, b) return a >= b end,
        ['lt'] = function(a, b) return a <  b end,
        ['le'] = function(a, b) return a <= b end,
    }

    local function tonumbers(str)
        return table.imap(string.split(str, '.'), function (i)
                local num = tonumber(i)
                assert(num, string.format("unable to convert '%s' to number in version string '%s'", i, str))
                return num
            end)
    end

    local function tostatus(result)
        return result and 0 or 1
    end

    local op = ops[op_arg]; assert(op, 'invalid comparison operator: '..op_arg)
    local va = tonumbers(va_arg); assert(#va > 0)
    local vb = tonumbers(vb_arg); assert(#vb > 0)

    for i = 1, math.max(#va, #vb) do
        local a, b = va[i] or 0, vb[i] or 0
        if a ~= b then return tostatus(op(a, b)) end
    end

    if op_arg == 'ne' or op_arg == 'gt' or op_arg == 'lt' then
        return tostatus(false)
    else
        return tostatus(true)
    end
end

function Jagen.command._debug(args)
    local Chunk = require 'Chunk'

    local ch = Chunk:new {a=1, 'v', b=2, 'z', _c=3}

    for k, v in ch:each() do
        print(k, v)
    end

end

function Jagen.nproc()
    if Jagen._nproc then return Jagen._nproc end
    local name, nproc = System.pread('*l', 'uname -s')
    if name == 'Darwin' then
        nproc = System.pread('*l', 'sysctl -n hw.ncpu')
    else
        nproc = System.pread('*l', 'nproc')
    end
    Jagen._nproc = tonumber(nproc)
    return Jagen._nproc
end

function Jagen:run(args)
    local first = args[1]
    if not first then
        return Jagen.command.help('usage')
    end
    if Jagen.command[first] then
        table.remove(args, 1)
        local status, rv = xpcall(Jagen.command[first], debug.traceback, args)
        if status == true then
            if rv == nil or rv == true or rv == 0 then
                return 0
            else
                return 1
            end
        else
            Log.error(rv)
        end
    else
        die("invalid command or argument '%s', try 'jagen help'", tostring(first))
    end
end

os.exit(Jagen:run(arg))
