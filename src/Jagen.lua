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

function Jagen.query(pkg, value, config)
    return Command:new('jagen-stage -q', value, pkg.name, config)
end

-- src

local function scm_packages(patterns)
    local packages = Package.load_rules()
    local o = {}

    if patterns and #patterns > 0 then
        for pattern in each(patterns) do
            local cpattern, found = string.convert_pattern(pattern), false
            for name, pkg in pairs(packages) do
                if name:match(cpattern) and pkg.source:is_scm() then
                    table.insert(o, pkg) found = true
                end
            end
            if not found then
                die('could not find source packages matching: %s', pattern)
            end
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

function Jagen.src.dirty(args)
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

function Jagen.src.status(args)
    local packages = scm_packages(args)
    for _, pkg in ipairs(packages) do
        local source = pkg.source
        if System.exists(source.dir) and System.exists(source:getscmdir()) then
            local head = source:head_name()
            local dirty = source:dirty() and ' dirty' or ''
            if #dirty > 0 and source.ignore_dirty then
                dirty = string.format(' dirty(ignored:%s)', source.ignore_dirty)
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

function Jagen.src.clean(args)
    local packages = scm_packages(args)
    for _, pkg in ipairs(packages) do
        local source = pkg.source
        Log.message('clean %s in %s', pkg.name, source.dir)
        if not pkg.source:clean() then
            die('failed to clean %s (%s) in %s',
                pkg.name, source:head_name(), source.dir)
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
            if not source.ignore_dirty and source:dirty() then
                die("could not update %s: source dir '%s' is dirty", pkg.name, dir)
            end
            local head_name = source:head_name()
            Log.message('updating %s (%s)', pkg.name, head_name)
            old_head = source:head()
            if not offline then
                if not source:update() then
                    die('failed to update %s (%s) in %s', pkg.name, head_name, dir)
                end
            end
            if not source:switch() then
                die('failed to switch %s (%s) to %s in %s', pkg.name, head_name, source:getrev(), dir)
            end
        else
            if offline then
                die("could not clone %s: offline mode", pkg.name)
            end
            Log.message('cloning %s from %s', pkg.name, source.location)
            if not source:clone() then
                die('failed to clone %s from %s to %s', pkg.name, source.location, dir)
            end
        end

        if not source:fixup() then
            die('failed to fix up %s in %s', pkg.name, dir)
        end

        if source:head() ~= old_head then
            assert(Target.from_args(pkg.name, 'unpack'):touch())
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

Jagen.command = {}

local function help_requested(args)
    return args and args[1] and
        (args[1] == '-h' or string.match(args[1], '^%--help$'))
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

function Jagen.clean_package(pkg, spec)
    local use = Target.from_use(spec)
    local config = use.config
    local build = pkg:get('build', config)
    if build then
        local source_dir = Jagen.query(pkg, 'source_dir', config):read()
        local build_dir = Jagen.query(pkg, 'build_dir', config):read()
        local clean_dirs = {}
        for dir in Jagen.query(pkg, 'build_clean', config):lines() do
            append(clean_dirs, dir)
        end
        if #clean_dirs == 0 and build_dir then
            clean_dirs = { build_dir }
        end
        for dir in each(clean_dirs) do
            if source_dir and System.same_dir(dir, source_dir) then
                if pkg.source and pkg.source:is_scm() then
                    local why = pkg.source:clean_disabled()
                    if why then
                        Log.message('not cleaning sources of %s: %s', pkg.name, why)
                    else
                        Jagen.src.clean { assert(use.name) }
                    end
                else
                    Log.message('not removing %s because it is the source dir of %s', dir, pkg.name)
                end
            elseif System.can_delete_safely(dir, source_dir) then
                Log.debug('removing %s', dir)
                System.rmrf(dir)
            end
        end
    end
    if config then
        Target.from_args(use.name, 'configure', config):remove()
    end
    return true
end

function Jagen.command.clean(args)
    local options = Options:new {
        { 'help,h' },
        { 'match,m' }
    }
    args = options:parse(args)
    if not args then return false end

    if args['help'] then
        return Jagen.command['help'] { 'clean' }
    end

    -- no targets specified, clean the whole project
    if #args == 0 then
        local clean_dirs = {
            'jagen_bin_dir',
            'jagen_build_dir',
            'jagen_include_dir',
            'jagen_log_dir',
            'jagen_host_dir',
            'jagen_target_dir',
            'jagen_cargo_config_dir',
        }
        return System.rmrf(unpack(System.getenv(clean_dirs))) and
               Jagen.command.refresh()
    end

    local packages, ok = Package.load_rules()
    if not ok then
        Log.error('aborting clean due to rule errors')
        return false
    end

    local targets, found = {}
    for i, pattern in iter(extend({}, args), map(string.to_target_pattern)) do
        for name, pkg in iter(packages) do
            for config, this in pkg:each_config() do
                local spec = string.format('%s:%s', name, config)
                if spec:match(pattern) then
                    targets[spec] = pkg
                    found = true
                end
            end
        end
        if not found then
            Log.warning('could not find targets matching: %s', args[i])
        end
    end

    if args['match'] then
        for spec, _ in pairs(targets) do
            print(spec)
        end
        return true
    end

    if not found then return false end 

    -- clean_package uses query, need to regenerate includes beforehand
    if not Jagen.command.refresh() then return false end

    for spec, pkg in pairs(targets) do
        if not Jagen.clean_package(pkg, spec) then
            return false
        end
    end

    return true
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

local function generate_cargo_config(packages)
    local targets, lines = {}, {}
    local function do_generate(pkg)
        for config, this in pkg:each_config() do
            local build = pkg:get('build', config)
            if build and build.type == 'rust' and config ~= 'host' then
                local build_system = Jagen.query(pkg, 'build_system', config):read()
                if build_system then
                    local toolchain = assert(packages[build.toolchain])
                    local toolchain_build = toolchain:get('build', config)
                    local system = toolchain_build.system
                    local cc = toolchain_build.cc or build.cc or 'gcc'
                    if system and cc then
                        targets[build_system] = string.format('%s-%s', system, cc)
                    end
                else
                    Log.warning("could not determine a target system to build the package '%s' in '%s' config: "..
                        "verify that the selected toolchain specifies a system or set 'build.system' explicitly "..
                        "for this package%s", name, config, pkg:format_at())
                end
            end
        end
    end
    for name, pkg in pairs(packages) do
        do_generate(pkg)
    end
    for target, bin in pairs(targets) do
        table.insert(lines, string.format('[target.%s]\nlinker = "%s"', target, bin))
    end
    local config_dir = assert(os.getenv('jagen_cargo_config_dir'))
    local config_path = System.mkpath(config_dir, 'config')
    System.mkdir(config_dir)
    local file = assert(io.open(config_path, 'w'))
    file:write(table.concat(lines, '\n'), '\n')
    file:close()
end


function Jagen.command.refresh(args, packages)
    if help_requested(args) then
        return Jagen.command['help'] { 'refresh' }
    end

    prepare_root()

    local packages, ok = packages, true
    if not packages then
        packages, ok = Package.load_rules()
    end
    local Script = require 'Script'
    local include_dir = assert(os.getenv('jagen_include_dir'))
    local log_dir = assert(os.getenv('jagen_log_dir'))

    local pipe = Command:new('find "$jagen_include_dir"')
                        :append('-mindepth 1 -maxdepth 1')
                        :append('| grep -v ":export"'):popen()
    System.rmrf(unpack(aslist(pipe:lines()))) pipe:close()

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
    generate_cargo_config(packages)

    local names = {}
    for name, _ in pairs(packages) do
        table.insert(names, name)
    end
    table.sort(names)

    local names_file = assert(io.open(System.mkpath(Jagen.build_dir, '.jagen-names'), 'wb'))
    local scm_names_file = assert(io.open(System.mkpath(Jagen.build_dir, '.jagen-scm-names'), 'wb'))
    local configs_file = assert(io.open(System.mkpath(Jagen.build_dir, '.jagen-configs'), 'wb'))
    local targets_file = assert(io.open(System.mkpath(Jagen.build_dir, '.jagen-targets'), 'wb'))
    local stages, cstages, configs = {}, {}, {}

    for _, name in ipairs(names) do
        local pkg = packages[name]
        assert(names_file:write(string.format('%s\n', name)))
        if pkg.source and pkg.source:is_scm() then
            assert(scm_names_file:write(string.format('%s\n', name)))
        end
        for config, _ in pairs(pkg.configs or {}) do
            configs[config] = true
            assert(configs_file:write(string.format('%s:%s\n', name, config)))
            assert(targets_file:write(string.format('%s::%s\n', name, config)))
        end
        assert(configs_file:write(string.format('%s\n', name)))
        for target in pkg:each() do
            if target.config then
                cstages[target.stage] = true
            else
                stages[target.stage] = true
            end
            assert(targets_file:write(string.format('%s\n', tostring(target))))
        end
    end
    for stage in pairs(stages) do
        assert(targets_file:write(string.format(':%s\n', stage)))
    end
    for stage in pairs(cstages) do
        for config in pairs(configs) do
            assert(targets_file:write(string.format(':%s:%s\n', stage, config)))
        end
    end
    for config in pairs(configs) do
        assert(targets_file:write(string.format('::%s\n', config)))
    end

    names_file:close()
    scm_names_file:close()
    configs_file:close()
    targets_file:close()

    return ok
end

function Jagen.command.build(args)
    local options = Options:new {
        { 'help,h' },
        { 'match,m' },
        { 'clean,c' },
        { 'all,a' },
        { 'no-rebuild,n' },
        { 'progress,p' },
        { 'quiet,q' },
    }
    args = options:parse(args)
    if not args then return false end
    if args['help'] then
        return Jagen.command['help'] { 'build' }
    end

    local packages, ok = Package.load_rules()
    if not ok then
        Log.error('aborting the build due to rule errors')
        return false
    end

    local targets, to_clean, arg_clean = {}, {}, args['clean']
    for i, pattern in iter(extend({}, args), map(string.to_target_pattern)) do
        local found = false
        for name, pkg in iter(packages) do
            for target, this in pkg:each() do
                local stage = tostring(target)
                if stage:match(pattern) then
                    append(targets, stage) found = true
                    if arg_clean then
                        to_clean[tostring(this)] = pkg
                    end
                end
            end
        end
        if not found then
            Log.warning('could not find targets matching: %s', args[i])
        end
    end

    if args['match'] then
        for target in each(targets) do
            print(target)
        end
        return true
    end

    -- some targets were specified but none matched, consider this an error
    if #args > 0 and #targets == 0 then
        return false
    end

    Jagen.command.refresh(nil, packages)

    for spec, pkg in pairs(to_clean) do
        if not Jagen.clean_package(pkg, spec) then
            return false
        end
    end

    local args_path = System.mkpath(Jagen.build_dir, '.build-args')
    local args_file = assert(io.open(args_path, 'w'))
    if args._args then
        args_file:write(table.concat(args._args, '\n'))
    end
    args_file:close()
    local ok = Command:new(quote(Jagen.cmd), 'build', tostring(args), unpack(targets)):exec()
    io.open(args_path, 'w'):close()
    return ok
end

function Jagen.command.src(args)
    local first = args[1]
    local command = Jagen.src[first]
    if first then
        if command then
            table.remove(args, 1)
            return command(args)
        else
            die('invalid src subcommand: %s', first)
        end
    else
        return Jagen.command.help { 'src' }
    end
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
        io.write(pkg.name, pkg:format_contexts(start_col, start_col - #pkg.name), '\n')
    end

    return true
end

function Jagen.command.image(args)
    if #args == 0 or help_requested(args) then
        return Jagen.command['help'] { 'image' }
    end
    return Command:new(quote(System.mkpath(Jagen.dir, 'src', 'cmd.sh')),
        'image', quote(unpack(args))):exec()
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
    local first = args[1]
    if not first then
        return Jagen.command.help('usage')
    end
    if Jagen.command[first] then
        table.remove(args, 1)
        local status =  Jagen.command[first](args)
        if status == nil or status == true or status == 0 then
            return 0
        else
            return 1
        end
    else
        die("invalid command or argument '%s', try 'jagen help'", tostring(first))
    end
end

os.exit(Jagen:run(arg))
