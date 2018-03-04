require 'common'

local System  = require 'System'
local Package = require 'Package'
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

Jagen =
{
    dir         = os.getenv('jagen_dir'),
    project_dir = os.getenv('jagen_project_dir'),

    shell = os.getenv('jagen_shell'),
    flags = os.getenv('jagen_flags'),

    build_dir = assert(os.getenv('jagen_build_dir')),
}

Jagen.cmd = System.mkpath(Jagen.dir, 'src', 'cmd.sh')
Jagen.pager = os.getenv('jagen_pager') or os.getenv('PAGER') or 'less'

function Jagen.flag(f)
    for w in string.gmatch(Jagen.flags, "[_%w]+") do
        if w == f then
            return true
        end
    end
    return false
end

-- src

local function scm_packages(names)
    local packages = Package.load_rules()
    local o = {}

    if names and #names > 0 then
        for _, name in ipairs(names) do
            if not packages[name] then
                die('no such package: %s', name)
            end
            if not packages[name].source:is_scm() then
                die('not scm package: %s', name)
            end
            table.insert(o, packages[name])
        end
    else
        for _, pkg in pairs(packages) do
            if pkg.source:is_scm() then
                table.insert(o, pkg)
            end
        end
    end

    table.sort(o, function (a, b)
            return a.name < b.name
        end)

    return o
end

Jagen.src = {}

-- Should return 0 if true, 1 if false, for shell scripting.
function Jagen.src.dirty(args)
    local packages = scm_packages(args)
    for _, pkg in ipairs(packages) do
        local source = pkg.source
        if System.exists(source.dir) and not System.is_empty(source.dir) and
                source:dirty() then
            return 0
        end
    end
    return 1
end

function Jagen.src.status(args)
    local packages = scm_packages(args)
    for _, pkg in ipairs(packages) do
        local source = pkg.source
        if System.exists(source.dir) and not System.is_empty(source.dir) then
            local head = source:head()
            if not head then
                die('failed to get source head for %s in %s',
                    pkg.name, source.dir)
            end
            local dirty = source:dirty() and ' dirty' or ''
            if #dirty > 0 and source.ignore_dirty then
                dirty = dirty..'(ignored)'
            end
            local exclude = source.exclude and ' excluded' or ''
            print(string.format("%s (%s): %s%s%s", pkg.name, source.location, head,
                dirty, exclude))
        else
            print(string.format("%s (%s): not exists", pkg.name, source.location))
        end
    end
end

function Jagen.src.clean(args)
    local packages = scm_packages(args)
    for _, pkg in ipairs(packages) do
        local source = pkg.source
        Log.message('clean %s in %s', pkg.name, source.dir)
        if not pkg.source:clean() then
            die('failed to clean %s (%s) in %s',
                pkg.name, source:rev(), source.dir)
        end
    end
end

function Jagen.src.update(args)
    local packages = scm_packages(args)
    local offline = Jagen.flag 'offline'

    -- Sorting from the shortest to the longest is needed for the case when the
    -- source directories are specified inside each other and we need to clone
    -- both: deeper one is cloned first, then clone complains about already
    -- existing non-empty directory.
    table.sort(packages, function (a, b)
            return a.source.dir < b.source.dir
        end)

    for pkg in each(packages) do
        local source = pkg.source
        local dir = System.expand(source.dir)
        local old_head

        if System.exists(dir) and not System.is_empty(dir) then
            local rev = source:rev() or ''
            if not source.ignore_dirty and source:dirty() then
                die("could not update %s: source dir '%s' is dirty",
                    pkg.name, dir)
            end
            Log.message('updating %s (%s)', pkg.name, rev)
            old_head = source:head()
            if not offline then
                if not source:update() then
                    die('failed to update %s (%s) in %s', pkg.name, rev, dir)
                end
            end
            if not source:switch() then
                die('failed to switch %s to %s in %s', pkg.name, rev, dir)
            end
        else
            if offline then
                die("could not clone %s: offline mode", pkg.name)
            end
            Log.message('cloning %s from %s', pkg.name, source.location)
            if not source:clone() then
                die('failed to clone %s from %s to %s',
                    pkg.name, source.location, dir)
            end
        end

        if not source:fixup() then
            die('failed to fix up %s in %s', pkg.name, dir)
        end

        if source:head() ~= old_head then
            assert(Target:new(pkg.name, 'unpack'):touch())
        end
    end
end

function Jagen.src.delete(args)
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

function Jagen.src.each(args)
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

-- these should return status number or nothing (nil)

Jagen.command = {}

local function is_option(arg)
    return string.sub(arg, 1, 1) == '-'
end

local function help_requested(args)
    return args and args[1] and
        (args[1] == '-h' or string.match(args[1], '^%--help$'))
end

local function find_options(args)
    local options, rest = {}, {}
    for i = 1, #args do
        local arg = args[i]
        if is_option(arg) then
            table.insert(options, arg)
        else
            table.insert(rest, arg)
        end
    end
    return options, rest
end

local function complex_command(command, args)
    if help_requested(args) then
        return Jagen.command['help'] { command }
    end

    local subcommand = args[1]
    table.remove(args, 1)

    if not subcommand then
        die("command required, try 'jagen %s help'", command)
    elseif Jagen[command][subcommand] then
        return Jagen[command][subcommand](args)
    else
        die("'%s' is not valid %s command, try 'jagen %s help'",
            subcommand, command, command)
    end
end

local function split_pkg_arg(arg)
    local match = string.gmatch(arg, '[^:]+')
    local name, config = match(), match()
    return name, config
end

function Jagen.command.help(args)
    section = args[1] or 'usage'
    local help = require 'help'

    if help[section] then
        io.write(help[section])
    else
        Log.error('no such help section: %s', section)
        return 2
    end
end

function Jagen.command.get(args)
    local packages = Package.load_rules()
    local pkg_arg = args[1]
    local pkg_var = args[2]

    if not pkg_arg then
        die('the package to query is not specified')
    end
    if not pkg_var then
        die('the variable to get is not specified')
    end

    local name, config = split_pkg_arg(pkg_arg)
    local pkg = packages[name]

    if not pkg then
        die('no such package: %s', name)
    end

    local result = pkg:query(pkg_var, config)
    local count = 0
    for k, v in pairs(result) do
        count = count + 1
    end
    if count == 1 then
        local _, value = next(result)
        print(value)
    else
        for k, v in pairs(result) do
            print(string.format('%s: %s', k, v))
        end
    end

    return 0
end

local function clean_packages(args)
    local packages = Package.load_rules()

    function force_reconfigure(name, config)
        assert(Target:new(name, 'configure', config):remove())
    end

    for _, arg in ipairs(args) do
        local match = string.gmatch(arg, '[^:]+')
        local name, config = match(), match()
        local pkg = packages[name]

        if not pkg then
            die('no such package: %s', name)
        end

        if pkg.build and pkg.build.in_source and pkg.source:is_scm() then
            Jagen.src.clean({ name })
        else
            for _, dir in pairs(pkg:query('build_dir', config)) do
                assert(System.rmrf(dir))
            end
        end

        if config then
            force_reconfigure(name, config)
        else
            if next(pkg.configs) then
                for config in pairs(pkg.configs) do
                    force_reconfigure(name, config)
                end
            else
                force_reconfigure(name)
            end
        end
    end
end

local function clean_project()
    local clean_dirs = {
        'jagen_bin_dir',
        'jagen_build_dir',
        'jagen_include_dir',
        'jagen_log_dir',
        'jagen_host_dir',
        'jagen_target_dir',
    }
    assert(System.rmrf(table.unpack(System.getenv(clean_dirs))))
end

function Jagen.command.clean(args)
    if help_requested(args) then
        return Jagen.command['help'] { 'clean' }
    end

    if #args > 0 then
        clean_packages(args)
    else
        clean_project()
    end

    return Jagen.command.refresh()
end

local function prepare_root()
    local create_dirs = {
        'jagen_bin_dir',
        'jagen_build_dir',
        'jagen_include_dir',
        'jagen_log_dir',
    }
    assert(System.mkdir(table.unpack(System.getenv(create_dirs))))
end

function Jagen.command.refresh(args, packages)
    if help_requested(args) then
        return Jagen.command['help'] { 'refresh' }
    end

    prepare_root()

    local packages = packages or Package.load_rules()
    local Script = require 'Script'
    local include_dir = assert(os.getenv('jagen_include_dir'))
    local log_dir = assert(os.getenv('jagen_log_dir'))

    for _, pkg in pairs(packages) do
        pkg:add_ordering_dependencies()
        Script:generate(pkg, include_dir)

        for stage in pkg:each() do
            local filename = string.format('%s/%s.log', log_dir, tostring(stage))
            assert(io.open(filename, 'a+')):close()
        end
    end

    local build_file = System.mkpath(Jagen.build_dir, 'build.ninja')
    Ninja.generate(build_file, packages)

    local names = {}
    for name, _ in pairs(packages) do
        table.insert(names, name)
    end
    table.sort(names)

    local names_file = assert(io.open(System.mkpath(Jagen.build_dir, '__package_names'), 'wb'))
    local scm_names_file = assert(io.open(System.mkpath(Jagen.build_dir, '__package_names_scm'), 'wb'))
    local configs_file = assert(io.open(System.mkpath(Jagen.build_dir, '__package_configs'), 'wb'))
    local targets_file = assert(io.open(System.mkpath(Jagen.build_dir, '__package_targets'), 'wb'))

    for _, name in ipairs(names) do
        local pkg = packages[name]
        assert(names_file:write(string.format('%s\n', name)))
        if pkg.source and pkg.source:is_scm() then
            assert(scm_names_file:write(string.format('%s\n', name)))
        end
        for config, _ in pairs(pkg.configs or {}) do
            assert(configs_file:write(string.format('%s:%s\n', name, config)))
        end
        assert(configs_file:write(string.format('%s\n', name)))
        for target in pkg:each() do
            assert(targets_file:write(string.format('%s\n', target:__tostring(':'))))
        end
    end

    names_file:close()
    scm_names_file:close()
    configs_file:close()
    targets_file:close()
end

local function find_targets(packages, arg)
    local targets = {}

    local function match_config(a, b)
        return not a.config or a.config == b.config
    end
    local function match_stage(a, b)
        return not a.stage or a.stage == b.stage
    end
    local function match_target(target, stage)
        return match_stage(target, stage) and match_config(target, stage)
    end

    local target = Target:from_arg(arg)
    local packages = target.name and { packages[target.name] } or packages

    for _, pkg in pairs(packages) do
        for stage in pkg:each() do
            if match_target(target, stage) then
                table.insert(targets, stage)
            end
        end
    end

    if #targets == 0 then
        die('could not find targets matching argument: %s', arg)
    end

    return targets
end

function Jagen.command.build(args)
    if help_requested(args) then
        return Jagen.command['help'] { 'build' }
    end

    local packages = Package.load_rules()
    Jagen.command.refresh(nil, packages)
    local options, rest = find_options(args)

    for _, arg in ipairs(rest) do
        for _, target in ipairs(find_targets(packages, arg)) do
            table.insert(options, target)
        end
    end

    return Command:new(quote(System.mkpath(Jagen.dir, 'src', 'cmd.sh')),
        'build', quote(unpack(options))):exec()
end

function Jagen.command.src(args)
    return complex_command('src', args)
end

function Jagen.command.show(args)
    if #args == 0 or help_requested(args) then
        return Jagen.command['help'] { 'show' }
    end

    if #args ~= 1 then
        die('expected a single argument but given %d', #args)
    end

    local filename = tostring(Target:from_arg(args[1]))
    if #filename == 0 then
        die('the target name is empty')
    end

    local path = System.mkpath(assert(os.getenv('jagen_log_dir')),
                               filename..'.log')
    return os.execute(string.format('%s "%s"', Jagen.pager, path))
end

function Jagen.command.list(args)
    if #args == 0 or help_requested(args) then
        return Jagen.command['help'] { 'list' }
    end

    if args[1] ~= 'packages' then
        die("invalid list command '"..args[1].."', try 'jagen list help'")
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

    local packages = Package.load_rules()
    local pkg_list, name_max = {}, 0
    for name, pkg in pairs(packages) do
        if #name > name_max then name_max = #name end
        table.insert(pkg_list, pkg)
    end
    table.sort(pkg_list, function (a, b) return a.name < b.name end)

    local dirname = System.dirname(Jagen.project_dir)
    local col2_pos = name_max + 2

    local function format_context(c, level)
        level = level or 0
        local s = ''
        local function name()
            local s = ''
            if c.name then
                s = string.format('%s', c.name)
            end
            if c.config then
                s = string.format('%s:%s', s, c.config)
            end
            return s
        end
        local function filename()
            local s = ''
            if c.filename then
                local name, rem = string.remove_prefix(c.filename, dirname)
                s = string.format('%s%s', rem and '...' or '', name)
                if c.line then
                    s = string.format('%s:%d', s, c.line)
                end
            end
            return s
        end
        if c.name and c.filename then
            s = string.format('%s (%s)', name(), filename())
        elseif c.name then
            s = name()
        elseif c.filename then
            s = filename()
        end
        if c.implicit and #s > 0 then
            s = string.format('%s *', s)
        end
        if #s > 0 then
            s = string.format('%s%s', string.rep(' ', level * 2), s)
        end
        return s
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

    -- prune duplicate context entries
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
        io.write(pkg.name)
        io.write(string.rep(' ', col2_pos - #pkg.name))
        local lines = {}
        for _, context in ipairs(pkg.contexts) do
            local level = 0
            while context do
                if not show_all and context.implicit then break end
                local str = format_context(context, level)
                if #str > 0 then
                    table.insert(lines, str)
                end
                context = context.parent
                level = level + 1
            end
        end
        io.write(table.concat(lines, '\n'..string.rep(' ', col2_pos)), '\n')
    end
end

function Jagen.command.image(args)
    if help_requested(args) then
        return Jagen.command['help'] { 'image' }
    end

    return Command:new(quote(System.mkpath(Jagen.dir, 'src', 'cmd.sh')),
        'image', quote(unpack(args)))
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
    local cmd = args[1]

    if #args == 0 or help_requested(args) then
        return Jagen.command['help']({ args[2] })
    elseif Jagen.command[cmd] then
        table.remove(args, 1)
        return Jagen.command[cmd](args)
    else
        die("invalid command or argument '%s', try 'jagen help'", cmd)
    end
end

os.exit(Jagen:run(arg) and 0 or 1)
