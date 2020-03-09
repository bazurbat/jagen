require 'common'

local System  = require 'System'
local Rules   = require 'Rules'
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

    has_console = os.getenv('jagen__has_console')
}

Jagen.cmd = System.mkpath(Jagen.dir, 'src', 'cmd.sh')
Jagen.pager = os.getenv('jagen_pager') or os.getenv('PAGER') or 'less'
Jagen.build_file = System.mkpath(Jagen.build_dir, 'build.ninja')
Jagen.build_auto_packages_file = System.mkpath(Jagen.build_dir, '.auto-packages')
Jagen.build_targets_file = System.mkpath(Jagen.build_dir, '.build-targets')

function Jagen.flag(f)
    for w in string.gmatch(Jagen.flags, "[_%w]+") do
        if w == f then
            return true
        end
    end
    return false
end

function Jagen:find_in_path(pathname)
    Log.debug2('find_in_path: %s', pathname)
    local tried_paths = {}
    for dir in each(Jagen:path()) do
        local path = System.mkpath(dir, pathname)
        append(tried_paths, path)
        local file = io.open(path)
        if file then
            file:close()
            return path, tried_paths
        end
    end
    return nil, tried_paths
end

function Jagen:path()
    if self._path then return self._path end
    local path = {}
    for item in string.gmatch(os.getenv('jagen_path') or '', '[^\t\n]+') do
        append(path, item)
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
    local packages = Rules:load()
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

function Jagen.source.clean(args)
    local options = Options:new {
        { 'ignored,i' },
        { 'ignore-dirty,y' },
        { 'ignore-exclude,Y' }
    }
    args = options:parse(args)
    if not args then return false end

    local packages, ok = scm_packages(args), true
    local force_exclude = os.getenv('jagen__force_exclude')
    local clean_ignored = args['ignored'] or os.getenv('jagen__clean_ignored')
    local ignore_dirty = args['ignore-dirty'] or os.getenv('jagen__ignore_dirty')
    local ignore_exclude = args['ignore-exclude'] or os.getenv('jagen__ignore_exclude')

    function clean(source, ignored)
        local ok = source:clean(ignored)
        if ok then
            assert(Target.from_args(assert(source.name), 'clean'):touch())
        end
        return ok
    end

    for pkg in each(packages) do
        local source = pkg.source
        if source:exists() then
            local dir = System.expand(source.dir)
            if force_exclude or not ignore_exclude and source.exclude then
                Log.message("not cleaning %s: the source is excluded", pkg.name)
            elseif source:dirty() then
                ignore_dirty = ignore_dirty and 'forced' or source.ignore_dirty
                if ignore_dirty then
                    Log.message("cleaning %s: ignoring dirty status of '%s' (%s)", pkg.name, dir, tostring(ignore_dirty))
                    ok = clean(source, clean_ignored)
                else
                    Log.warning("not cleaning %s: the source directory '%s' has unsaved changes", pkg.name, dir)
                    ok = false
                end
            else
                Log.message("cleaning %s: %s", pkg.name, dir)
                ok = clean(source, clean_ignored)
            end
        end
    end

    return ok
end

function Jagen.source.update(args)
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

    local packages = Rules:load()
    if not Rules:validate() then
        Log.error('aborting clean due to rule errors')
        return false
    end

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
        if not found then
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
            'jagen_host_dir',
            'jagen_target_dir',
            'jagen_cargo_config_dir',
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

local function prepare_root()
    local create_dirs = {
        'jagen_bin_dir',
        'jagen_build_dir',
        'jagen_include_dir',
        'jagen_log_dir',
    }
    assert(System.mkdir(table.unpack(System.getenv(create_dirs))))
    System.create_file(Jagen.build_auto_packages_file)
    System.create_file(Jagen.build_targets_file)
end

local function generate_cargo_config(packages)
    local targets, lines = {}, {}
    for name, pkg in pairs(packages) do
        for this, config in pkg:each_config() do
            local build = this.build
            if build.type == 'rust' then
                local system = pkg:get_build('system', config)
                local cc = pkg:get_build('cc', config)
                    or pkg:get_toolchain_build('cc', config, packages)
                    or 'gcc'
                local toolchain_system = pkg:get_toolchain_build('system', config, packages)
                if system and cc and toolchain_system then
                    targets[system] = string.format('%s-%s', toolchain_system, cc)
                end
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

local function write_auto_packages(targets)
    table.sort(targets)
    local file = assert(io.open(Jagen.build_auto_packages_file, 'w'))
    for target in each(targets) do
        file:write(tostring(target), '\n')
    end
    file:close()
end

local function read_auto_packages()
    local targets = {}
    local file = io.open(Jagen.build_auto_packages_file, 'r')
    if file then
        for line in file:lines() do
            append(targets, Target.from_use(line))
        end
        file:close()
    end
    return targets
end

function Jagen.command.refresh(args, packages)
    if help_requested(args) then
        return Jagen.command['help'] { 'refresh' }
    end

    prepare_root()

    local packages, ok = packages, true
    if not packages then
        packages = Rules:load(read_auto_packages())
    end
    local Script = require 'Script'
    local include_dir = assert(os.getenv('jagen_include_dir'))
    local log_dir = assert(os.getenv('jagen_log_dir'))

    Command:new('find "$jagen_include_dir"')
           :append('-mindepth 1 -maxdepth 1')
           :append('\\! \\( -name "*:export.*" -o -name "*:export:*" \\)')
           :append('-delete'):exec()

    for _, pkg in pairs(packages) do
        Rules:add_patch_dependencies(pkg)
        Rules:add_files_dependencies(pkg)
        Rules:add_ordering_dependencies(pkg)
        Script:generate(pkg, include_dir)

        for stage in pkg:each() do
            local filename = string.format('%s/%s.log', log_dir, tostring(stage))
            assert(io.open(filename, 'a+')):close()
        end
    end

    do
        local file = io.open(Jagen.build_targets_file, 'r')
        if file then
            for line in file:lines() do
                local target = Target:from_arg(line)
                for name, pkg in pairs(packages) do
                    if name == pkg.name then
                        for stage in pkg:each() do
                            if stage == target then
                                stage.interactive = true
                            end
                        end
                    end
                end
            end
            file:close()
        end
    end

    Ninja.generate(Jagen.build_file, packages)
    if Rules:have_rust() then
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

    return Rules:validate()
end

local function write_targets(targets, args)
    local is_interactive = Jagen.has_console and not args['quiet'] and not (args['progress'] or args['follow'] or args['follow-all'])
    local curr_list, saved_list, is_eq = {}, {}, true

    if is_interactive then
        for target, explicit in pairs(targets) do
            if explicit then
                append(curr_list, target)
            end
        end
        sort(curr_list)
    end

    local file = io.open(Jagen.build_targets_file)
    if file then
        for line in file:lines() do
            append(saved_list, line)
        end
        file:close()
    end

    if #curr_list ~= #saved_list then
        is_eq = false
    else
        for i = 1, #curr_list do
            if curr_list[i] ~= saved_list[i] then
                is_eq = false
                break
            end
        end
    end

    if not is_eq then
        local file = assert(io.open(Jagen.build_targets_file, 'w'))
        for target in each(curr_list) do
            file:write(target, '\n')
        end
        file:close()
    end
end

function Jagen.command.build(args)
    local options = Options:new {
        { 'help,h' },
        { 'match,m' },
        { 'clean,c' },
        { 'clean-ignored,C' },
        { 'all,a' },
        { 'no-rebuild,n' },
        { 'progress,p' },
        { 'follow,f' },
        { 'follow-all,F' },
        { 'quiet,q' },
        { 'exclude,x' },
        { 'ignore-dirty,y' },
        { 'ignore-exclude,Y' },
        { 'no-auto' }
    }
    args = options:parse(args)
    if not args then return false end
    if args['help'] then
        return Jagen.command['help'] { 'build' }
    end

    local packages = Rules:load()
    if not Rules:validate() then
        Log.error('aborting the build due to rule errors')
        return false
    end

    local targets, arg_clean = {}, args['clean'] or args['clean-ignored']

    if #args == 0 then
        if not (args['progress'] or args['follow'] or args['follow-all']) then
            args['quiet'] = true
        end
        for name in pairs(packages) do
            append(args, name)
        end
    end

    local function match(s, p)
        return not s and not p or
               p and #p == 0 or
               s and p and s:match(p)
    end

    local not_found_args = {}
    for arg in each(args) do
        local found = false
        local parts = arg:split(':')
        local arg_stage = parts[2]
        local name_pat, stage_pat, config_pat = unpack(map(string.to_pattern, parts))

        local function by_name(pkg)
            return match(pkg.name, name_pat)
        end
        local function add(target)
            targets[tostring(target)] = true found = true
        end

        for name, pkg in filter(by_name)(pairs(packages)) do
            for target in pkg:each() do
                if arg_stage then
                    if target.stage == 'clean' then
                        if (arg_clean or arg_stage == 'clean') and match(target.config, config_pat) then
                            add(target)
                        end
                    elseif match(target.stage, stage_pat) and match(target.config, config_pat) then
                        add(target)
                    end
                else
                    if target.stage == 'clean' then
                        if arg_clean then
                            add(target)
                        end
                    else
                        add(target)
                    end
                end
            end
            if not found then
                append(not_found_args, arg)
            end
        end
    end

    for arg in each(not_found_args) do
        Log.warning('could not find targets matching: %s', arg)
    end

    if args['match'] then
        local keys = table.keys(targets)
        table.sort(keys)
        for key in each(keys) do
            local explicit = targets[key]
            print(string.format('%s%s', key, not explicit and ' *' or ''))
        end
        return true
    end

    -- some targets were specified but none matched, consider this an error
    if #args > 0 and not next(targets) then
        return false
    end

    -- if not table.iequal(read_auto_packages(), new_auto_pkgs) then
    --     write_auto_packages(new_auto_pkgs)
    -- end

    write_targets(targets, args)

    local args_path = System.mkpath(Jagen.build_dir, '.build-args')
    local args_file = assert(io.open(args_path, 'w'))
    if args._args then
        args_file:write(table.concat(args._args, '\n'))
    end
    args_file:close()
    local ok = Command:new(quote(Jagen.cmd), 'build', tostring(args), unpack(table.keys(targets))):exec()
    io.open(args_path, 'w'):close()
    return ok
end

Jagen.command['rebuild'] = Jagen.command.build
Jagen.command['re'] = Jagen.command.build
Jagen.command['do'] = Jagen.command.build

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

    local packages = Rules:load()
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
        io.write(pkg.name, Rules:format_contexts(pkg, start_col, start_col - #pkg.name), '\n')
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
