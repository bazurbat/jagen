require 'common'

local System  = require 'System'
local Package = require 'Package'
local Target  = require 'Target'
local Source  = require 'Source'
local Log     = require 'Log'
local Ninja   = require 'Ninja'

local function die(...)
    Log.error(...)
    os.exit(1)
end

Jagen =
{
    dir   = os.getenv('jagen_dir'),
    shell = os.getenv('jagen_shell'),
    flags = os.getenv('jagen_flags'),

    build_dir = assert(os.getenv('jagen_build_dir')),

    package_aliases = {}
}

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
                pkg.name, source.branch, source.dir)
        end
    end
end

function Jagen.src.update(args)
    local packages = scm_packages(args)
    local offline = Jagen.flag 'offline'
    -- Sorting from the shortest to the longest is needed for a case when the
    -- source directories are specified inside each other and we need to clone
    -- both, deeper one is cloned first, then clone complains about already
    -- existing directory or update fails.
    table.sort(packages, function (a, b)
            return a.source.dir < b.source.dir
    end)
    for _, pkg in ipairs(packages) do
        local source = pkg.source
        local old_head
        if System.exists(source.dir) and not System.is_empty(source.dir) then
            old_head = source:head()
            if not source:dirty() then
                if offline then
                    Log.message('switching %s to %s in %s',
                        pkg.name, source.branch, source.dir)
                else
                    Log.message('updating %s to %s in %s',
                        pkg.name, source.branch, source.dir)
                end

                if not offline then
                    if not source:update() then
                        die('failed to update %s from %s in %s',
                            pkg.name, source.location, source.dir)
                    end
                end

                if not source:switch() then
                    die('failed to switch %s to the latest %s in %s',
                        pkg.name, source.branch, source.dir)
                end
            else
                Log.warning("not updating %s because source directory '%s' is dirty",
                    pkg.name, source.dir)
            end
        else
            if offline then
                die("could not clone '%s' in offline mode", pkg.name)
            end
            Log.message('clone %s from %s to %s',
                pkg.name, source.location, source.dir)
            if not source:clone() then
                die('failed to clone %s from %s to %s',
                    pkg.name, source.location, source.dir)
            end
        end

        if not source:fixup() then
            die('failed to fix up %s source in %s',
                pkg.name, source.dir)
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

local project_dirs = {
    'jagen_bin_dir',
    'jagen_build_dir',
    'jagen_include_dir',
    'jagen_log_dir',
    'jagen_host_dir',
    'jagen_target_dir',
}

local function clean_project()
    assert(System.rmrf(table.unpack(System.getenv(project_dirs))))
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
    assert(System.mkdir(table.unpack(System.getenv(project_dirs))))
end

function Jagen.command.refresh(args, packages)
    if help_requested(args) then
        return Jagen.command['help'] { 'refresh' }
    end

    prepare_root()

    local packages = packages or Package.load_rules(true)
    local Script = require 'Script'
    local include_dir = assert(os.getenv('jagen_include_dir'))
    local log_dir = assert(os.getenv('jagen_log_dir'))

    for _, pkg in pairs(packages) do
        Script:generate(pkg, include_dir)

        -- create/truncate all log files beforehand to allow tail following
        -- them on interactive rebuild
        for stage in pkg:each() do
            local filename = string.format('%s/%s.log', log_dir, tostring(stage))
            assert(io.open(filename, 'w+')):close()
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

    local packages = Package.load_rules(true)
    Jagen.command.refresh(nil, packages)
    local options, rest = find_options(args)

    for _, arg in ipairs(rest) do
        for _, target in ipairs(find_targets(packages, arg)) do
            table.insert(options, target)
        end
    end

    local cmd = System.mkpath(Jagen.dir, 'src', 'cmd.sh')
    local err, status = System.exec('%s build %s', cmd,
        System.quote(table.unpack(options)))

    return status
end

function Jagen.command.src(args)
    return complex_command('src', args)
end

function Jagen.command.image(args)
    if help_requested(args) then
        return Jagen.command['help'] { 'image' }
    end

    local cmd = System.mkpath(Jagen.dir, 'src', 'cmd.sh')
    local err, status = System.exec('%s image %s', cmd,
        System.quote(table.unpack(args)))

    return status
end

function Jagen.command.test(args)
    if help_requested(args) then
        return Jagen.command['help'] { 'test' }
    end

    return 0
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

local function nproc()
    local name = System.pread('*l', 'uname -s')
    if name == 'Darwin' then
        return System.pread('*l', 'sysctl -n hw.ncpu')
    else
        return System.pread('*l', 'nproc')
    end
end

function Jagen:run(args)
    local cmd = args[1]

    Jagen.nproc = nproc()

    if #args == 0 or help_requested(args) then
        return Jagen.command['help']({ args[2] })
    elseif Jagen.command[cmd] then
        table.remove(args, 1)
        return Jagen.command[cmd](args)
    else
        die("invalid command or argument '%s', try 'jagen help'", cmd)
    end
end

os.exit(Jagen:run(arg) or 0)
