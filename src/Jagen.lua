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
    root_dir = os.getenv('jagen_root_dir'),

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

function Jagen:path()
    if self._path then return self._path end
    local path = {}
    for dir in Command:new(quote(Jagen.cmd), 'get_path'):read('*a'):gmatch('[^\t\n]+') do
        append(path, dir)
    end
    self._path = path
    return path
end

function Jagen:find_for_refresh()
    return Command:new(quote(Jagen.cmd), 'find_for_refresh'):aslist()
end

function Jagen._load_layers()
    local path, layers = Jagen:path(), {}
    assert(#path >= 2)
    table.remove(path)
    table.remove(path, 1)
    for dir in each(path) do
        local name = assert(dir:match('.*/(.+)'), 'item is not a path: '..dir)
        layers[name] = dir
    end
    return layers
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
            if pkg.source and pkg.source:is_scm() then
                table.insert(o, pkg)
            end
        end
    end

    table.sort(o, function (a, b)
            return a.name < b.name
        end)

    return o
end

Jagen.source = {}
Jagen.src = {
    dirty  = Jagen.source.dirty,
    status = Jagen.source.status,
    clean  = Jagen.source.clean,
    update = Jagen.source.update,
    delete = Jagen.source.delete,
    each   = Jagen.source.each,
}

function Jagen.source.dirty(args)
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

function Jagen.source.status(args)
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

function Jagen.source.clean(args)
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

function Jagen.source.update(args)
    local packages = scm_packages(args)
    local offline = Jagen.flag 'offline'
    local ok

    -- Sorting from the shortest to the longest is needed for the case when the
    -- source directories are specified inside each other and we need to clone
    -- both: deeper one is cloned first, then clone complains about already
    -- existing non-empty directory.
    table.sort(packages, function (a, b)
            return (a.source.dir or '') < (b.source.dir or '')
        end)

    if offline and #packages > 0 then
        Log.message('skipping fetch due to offline mode')
    end

    for pkg in each(packages) do
        local source = pkg.source
        local dir = System.expand(source.dir)
        local old_head

        if System.exists(dir) and not System.is_empty(dir) then
            if not source.ignore_dirty and source:dirty() then
                Log.warning("could not update %s: the source directory '%s' has unsaved changes", pkg.name, dir)
                ok = false
            else
                local head_name = source:head_name()
                Log.message('updating %s (%s)', pkg.name, head_name)
                old_head = source:head()
                if not source:update() then
                    Log.warning('failed to update %s (%s) in %s', pkg.name, head_name, dir)
                    ok = false
                end
            end
        else
            if offline then
                Log.warning("could not clone %s: offline mode", pkg.name)
                ok = false
            else
                Log.message('cloning %s from %s', pkg.name, source.location)
                if not source:clone() then
                    Log.warning('failed to clone %s from %s to %s', pkg.name, source.location, dir)
                    ok = false
                end
            end
        end

        if System.exists(dir) then
            if not source:fixup() then
                Log.warning('failed to fix up %s in %s', pkg.name, dir)
                ok = false
            end

            if source:head() ~= old_head then
                assert(Target.from_args(pkg.name, 'unpack'):touch())
            end
        end
    end

    return ok
end

function Jagen.source.delete(args)
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

function Jagen.source.each(args)
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

    local function argsub(p)
        return string.gsub(p, ':$', ':*')
    end

    local specs, found = {}
    for i, pattern in iter(extend({}, args), map(comp(argsub, string.convert_pattern))) do
        for name, pkg in iter(packages) do
            if name:match(pattern) then
                append(specs, name..':clean') found = true
            else
                for this, config in pkg:each_config() do
                    local spec = string.format('%s:%s', name, config)
                    if spec:match(pattern) then
                        append(specs, string.format('%s:clean:%s', name, config))
                        found = true
                    end
                end
            end
        end
        if not found then
            Log.warning('could not find targets matching: %s', args[i])
        end
    end

    if args['match'] then
        for spec in each(specs) do
            print(spec)
        end
        return true
    end

    if not found then return false end 

    return Jagen.command.build(specs)
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
    for this, config, pkg in Package.all_configs() do
        local build = this.build
        if build.type == 'rust' then
            local system = pkg:get_build('system', config)
            local cc = pkg:get_build('cc', config) or pkg:get_toolchain_build('cc', config) or 'gcc'
            local toolchain_system = pkg:get_toolchain_build('system', config)
            if system and cc and toolchain_system then
                targets[system] = string.format('%s-%s', toolchain_system, cc)
            end
        end
    end
    for target, path in pairs(targets) do
        table.insert(lines, string.format('[target.%s]\nlinker = "%s"', target, path))
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

    Command:new('find "$jagen_include_dir"')
           :append('-mindepth 1 -maxdepth 1')
           :append('\\! \\( -name "*:export.*" -o -name "*:export:*" \\)')
           :append('-delete'):exec()

    for _, pkg in pairs(packages) do
        pkg:add_patch_dependencies()
        pkg:add_files_dependencies()
        pkg:add_ordering_dependencies()
        Script:generate(pkg, include_dir)

        for stage in pkg:each() do
            local filename = string.format('%s/%s.log', log_dir, tostring(stage))
            assert(io.open(filename, 'a+')):close()
        end
    end

    local build_file = System.mkpath(Jagen.build_dir, 'build.ninja')
    Ninja.generate(build_file, packages)
    if Package.has_rust_rules then
        generate_cargo_config(packages)
    end

    local names = {}
    for name, _ in pairs(packages) do
        table.insert(names, name)
    end
    table.sort(names)

    local names_file = assert(io.open(System.mkpath(Jagen.build_dir, '.jagen-names'), 'wb'))
    local scm_names_file = assert(io.open(System.mkpath(Jagen.build_dir, '.jagen-scm-names'), 'wb'))
    local configs_file = assert(io.open(System.mkpath(Jagen.build_dir, '.jagen-configs'), 'wb'))
    local targets_file = assert(io.open(System.mkpath(Jagen.build_dir, '.jagen-targets'), 'wb'))
    local layers_file = assert(io.open(System.mkpath(Jagen.build_dir, '.jagen-layers'), 'wb'))
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
    local layers = Jagen._load_layers()
    for name, path in pairs(layers) do
        assert(layers_file:write(string.format('%s\n', name)))
    end

    names_file:close()
    scm_names_file:close()
    configs_file:close()
    targets_file:close()
    layers_file:close()

    return ok
end

function Jagen.command.build(args)
    local options = Options:new {
        { 'help,h' },
        { 'match,m' },
        { 'clean,c' },
        { 'interactive,i' },
        { 'all,a' },
        { 'no-rebuild,n' },
        { 'progress,p' },
        { 'follow,f' },
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

    local targets, arg_clean = {}, args['clean']

    for arg in each(args) do
        local found = false
        local namep, stagep = unpack(map(string.to_pattern, arg:split(':', 1)))
        for name, pkg in iter(packages, filter(function (_, name) return name:match(namep) end)) do
            for target in pkg:each() do
                if not stagep then
                    if arg_clean or target.stage ~= 'clean' then
                        targets[tostring(target)] = true found = true
                    end
                else
                    local stage = target:to_stage()
                    if stage:match(stagep) then
                        if target.stage ~= 'clean' or stagep:match('^%^clean%$$') or stagep:match('^%^clean%%%:') then
                            targets[tostring(target)] = true found = true
                        end
                        if arg_clean then
                            targets[tostring(Target.from_args(name, 'clean', target.config))] = true
                        end
                    end
                end
            end
        end
        if not found then
            Log.warning('could not find targets matching: %s', arg)
        end
    end

    if args['match'] then
        local keys = table.keys(targets)
        table.sort(keys)
        for target in each(keys) do
            print(target)
        end
        return true
    end

    -- some targets were specified but none matched, consider this an error
    if #args > 0 and not next(targets) then
        return false
    end

    local args_path = System.mkpath(Jagen.build_dir, '.build-args')
    local args_file = assert(io.open(args_path, 'w'))
    if args._args then
        args_file:write(table.concat(args._args, '\n'))
    end
    args_file:close()
    local ok
    if args['interactive'] then
        for key in pairs(targets) do
            local a = key:split(':')
            ok = Command:newf('jagen-stage -i %s %s %s', a[1] or "''", a[2] or "''", a[3] or "''"):exec()
            if not ok then return ok end
        end
    else
        ok = Command:new(quote(Jagen.cmd), 'build', tostring(args), unpack(table.keys(targets))):exec()
    end
    io.open(args_path, 'w'):close()
    return ok
end

function Jagen.command.source(args)
    local first = args[1]
    local command = Jagen.source[first]
    if first then
        if command then
            table.remove(args, 1)
            return command(args)
        else
            die('invalid source subcommand: %s', first)
        end
    else
        return Jagen.command.help { 'source' }
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
