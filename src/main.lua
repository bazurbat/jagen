require 'common'
require 'Ninja'

local system = require 'system'
local Pkg    = require 'Pkg'
local Target = require 'Target'
local Source = require 'Source'

jagen =
{
    dir  = os.getenv('jagen_dir'),
    root = os.getenv('jagen_root'),

    shell = os.getenv('jagen_shell'),

    debug = os.getenv('jagen_debug'),
    flags = os.getenv('jagen_flags'),

    lib_dir     = os.getenv('jagen_lib_dir'),
    src_dir     = os.getenv('jagen_src_dir'),
    build_dir   = os.getenv('jagen_build_dir'),
    include_dir = os.getenv('jagen_include_dir'),
}

jagen.cmd = system.mkpath(jagen.lib_dir, 'cmd.sh')
jagen.build_file = system.mkpath(jagen.build_dir, 'build.ninja')

function jagen.message(...)
    io.write('(I) ', string.format(...), '\n')
    io.flush()
end

function jagen.warning(...)
    io.stderr:write('(W) ', string.format(...), '\n')
    io.stderr:flush()
end

function jagen.error(...)
    io.stderr:write('(E) ', string.format(...), '\n')
    io.stderr:flush()
end

function jagen.debug0(...)
    if jagen.debug then
        io.write('(D) ', string.format(...), '\n')
        io.flush()
    end
end

function jagen.debug1(...)
    if jagen.debug >= '1' then
        io.write('(D) ', string.format(...), '\n')
        io.flush()
    end
end

function jagen.debug2(...)
    if jagen.debug >= '2' then
        io.write('(D) ', string.format(...), '\n')
        io.flush()
    end
end

function jagen.die(...)
    jagen.error(...)
    os.exit(1)
end

function jagen.flag(f)
    for w in string.gmatch(jagen.flags, "[_%w]+") do
        if w == f then
            return true
        end
    end
    return false
end

function jagen.touch(target)
    return system.exec('cd "$jagen_build_dir" && touch "%s"',
        tostring(target))
end

function jagen.remove(target)
    return system.exec('cd "$jagen_build_dir" && rm -f "%s"',
        tostring(target))
end

-- rules

local function load_rules()
    local env = { Pkg = Pkg }
    setmetatable(env, { __index = _G })
    local dirs = system.getenv { 'jagen_product_dir', 'jagen_root' }
    for _, dir in ipairs(dirs) do
        local filename = dir..'/rules.lua'
        if system.file_exists(filename) then
            local chunk = assert(loadfile(filename))
            setfenv(chunk, env)
            chunk()
        end
    end

    for _, pkg in pairs(Pkg.all) do
        pkg.source = Source:create(pkg.source, pkg.name)
    end

    return Pkg.all
end

-- src

jagen.src = {}

local function exists(pathname)
    assert(type(pathname) == 'string')
    return os.execute(string.format('test -e "%s"', pathname)) == 0
end

function jagen.src._touch(pkg)
    local target = Target:new(pkg.name, 'unpack')
    return system.exec('cd "$jagen_build_dir" && touch "%s"',
        tostring(target))
end

-- Should return 0 if true, 1 if false, for shell scripting.
function jagen.src.dirty(packages)
    for _, pkg in ipairs(packages) do
        local source = pkg.source
        if exists(source.dir) and source:dirty() then
            return 0
        end
    end
    return 1
end

function jagen.src.status(packages)
    for _, pkg in ipairs(packages) do
        local source = pkg.source
        if exists(source.dir) then
            local dirty = source:dirty() and 'dirty' or ''
            local head = source:head()
            if not head then
                jagen.die('failed to get source head for %s in %s',
                    pkg.name, source.dir)
            end
            print(string.format("%s (%s): %s %s", pkg.name, source.location, head, dirty))
        else
            print(string.format("%s (%s): not exists", pkg.name, source.location))
        end
    end
end

function jagen.src.clean(packages)
    for _, pkg in ipairs(packages) do
        local source = pkg.source
        jagen.message('clean %s in %s', pkg.name, source.dir)
        if not pkg.source:clean() then
            jagen.die('failed to clean %s (%s) in %s',
                pkg.name, source.branch, source.dir)
        end
    end
end

function jagen.src.update(packages)
    local offline = jagen.flag 'offline'
    -- Sorting from the shortest to the longest is needed for a case when the
    -- source directories are specified inside each other and we need to clone
    -- both, deeper one is cloned first, then clone complains about already
    -- existing directory or update fails.
    table.sort(packages, function (a, b)
            return a.source.dir < b.source.dir
    end)
    for _, pkg in ipairs(packages) do
        local source = pkg.source
        local old_head = source:head()
        if exists(source.dir) then
            if not source:dirty() then
                if offline then
                    jagen.message('switch %s to %s in %s',
                        pkg.name, source.branch, source.dir)
                else
                    jagen.message('update %s from %s to %s in %s',
                        pkg.name, source.location, source.branch, source.dir)
                end

                if not offline then
                    if not source:update() then
                        jagen.die('failed to update %s from %s in %s',
                            pkg.name, source.location, source.dir)
                    end
                end

                if not source:switch() then
                    jagen.die('failed to switch %s to the latest %s in %s',
                        pkg.name, source.branch, source.dir)
                end
            else
                jagen.warning("skip update of %s because working directory '%s' is dirty",
                    pkg.name, source.dir)
            end
        else
            if offline then
                jagen.die("could not clone '%s' in offline mode", pkg.name)
            elseif not source:clone() then
                jagen.die('failed to clone %s from %s to %s',
                    pkg.name, source.location, source.dir)
            end
        end

        if not offline and not source:fixup() then
            jagen.die('failed to fix up %s source in %s',
                pkg.name, source.dir)
        end

        if source:head() ~= old_head then
            jagen.src._touch(pkg)
        end
    end
end

function jagen.src.delete(packages)
    for _, pkg in ipairs(packages) do
        local source = pkg.source
        if exists(source.dir) then
            if not system.rmrf(source.dir) then
                jagen.die('failed to delete %s source directory %s',
                    pkg.name, source.dir)
            end
        end
    end
end

-- image

jagen.image = {}

function jagen.image.create(image_type)
    local Image = require 'Image'

    if image_type == 'rootfs' then
        local target_dir = assert(os.getenv('jagen_target_dir'))
        local out_file = '$jagen_build_dir/rootfs.ext4'

        Image:create(target_dir, out_file)
    elseif not image_type then
        jagen.die('image type is not specified')
    else
        jagen.die('unsupported image type: %s', image_type)
    end
end

-- these should return status number or nothing (nil)

jagen.command = {}

local function is_option(arg)
    return string.sub(arg, 1, 1) == '-'
end

local function help_requested(args)
    return args and (args[1] == '-h' or string.match(args[1] or '', '\--help$'))
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

function jagen.command.help(args)
    section = args[1] or 'usage'
    local help = require 'help'

    if help[section] then
        io.write(help[section])
    else
        jagen.error('no such help section: %s', section)
        return 2
    end
end

function jagen.command.clean(args)
    if help_requested(args) then
        return jagen.command['help'] { 'clean' }
    end

    if #args > 0 then
        local packages = load_rules()
        for _, arg in ipairs(args) do
            local match = string.gmatch(arg, '[^:]+')
            local name, config = match(), match()
            local pkg = packages[name]
            if not pkg then
                jagen.die('no such package: %s', name)
            end
            for config, dir in pairs(pkg:build_dirs(config)) do
                assert(system.rmrf(dir))
                assert(jagen.remove(Target:new(name, 'configure', config)))
            end
        end
        return 0
    end

    local vars = {
        'jagen_bin_dir',
        'jagen_build_dir',
        'jagen_include_dir',
        'jagen_log_dir',
        'jagen_host_dir',
        'jagen_target_dir',
    }
    local dirs = system.getenv(vars)

    assert(system.rmrf(unpack(dirs)))

    return jagen.command.refresh()
end

local function prepare_root()
    local vars = {
        'jagen_bin_dir',
        'jagen_build_dir',
        'jagen_include_dir',
        'jagen_log_dir'
    }
    local dirs = system.getenv(vars)

    assert(system.mkdir(unpack(dirs)))
end

function jagen.command.refresh(args)
    if help_requested(args) then
        return jagen.command['help'] { 'refresh' }
    end

    prepare_root()

    local packages = load_rules()
    local script = require 'script'
    local include_dir = assert(os.getenv('jagen_include_dir'))
    local log_dir = assert(os.getenv('jagen_log_dir'))

    for _, pkg in pairs(packages) do
        pkg:add_ordering_dependencies()

        script:write(pkg, include_dir)

        -- create/truncate all log files beforehand to allow tail following
        -- them on interactive rebuild
        for stage in pkg:each() do
            local filename = string.format('%s/%s.log', log_dir, tostring(stage))
            assert(io.open(filename, 'w+')):close()
        end
    end

    local ninja = Ninja:new()
    ninja:generate(jagen.build_file, packages)
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
        jagen.die('could not find targets matching argument: %s', arg)
    end

    return targets
end

function jagen.command.build(args)
    if help_requested(args) then
        return jagen.command['help'] { 'build' }
    end

    local packages = load_rules()
    local options, rest = find_options(args)

    for _, arg in ipairs(rest) do
        for _, target in ipairs(find_targets(packages, arg)) do
            table.insert(options, target)
        end
    end

    local err, status = system.exec('%s build %s', jagen.cmd,
        system.quote(unpack(options)))

    return status
end

function jagen.command.status()
    return jagen.command.src({}, { 'status' })
end

local function scm_packages(names)
    local packages = load_rules()
    local o = {}

    if names and #names > 0 then
        for _, name in ipairs(names) do
            if not packages[name] then
                jagen.die('no such package: %s', name)
            end
            if not packages[name].source:is_scm() then
                jagen.die('not scm package: %s', name)
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

function jagen.command.src(args)
    if help_requested(args) then
        return jagen.command['help'] { 'src' }
    end

    local options, rest = find_options(args)
    local command = rest[1]
    table.remove(rest, 1)

    if not command then
        jagen.die("command required, try 'jagen src help'")
    elseif jagen.src[command] then
        local packages = scm_packages(rest)
        return jagen.src[command](packages)
    else
        jagen.die("'%s' is not valid src command, use 'jagen src help'", command)
    end
end

function jagen.command.image(options, rest)
    local command = rest[1]

    if not command then
        jagen.die("command required, try 'jagen image help'")
    elseif jagen.image[command] then
        return jagen.image[command](rest[2])
    else
        jagen.die("'%s' is not valid image command, use 'jagen image help'", command)
    end
end

function jagen:_nproc()
    local name = system.pread('*l', 'uname -s')
    if name == 'Darwin' then
        return system.pread('*l', 'sysctl -n hw.ncpu')
    else
        return system.pread('*l', 'nproc')
    end
end

function jagen:run(args)
    local cmd = args[1]

    jagen.nproc = jagen:_nproc()

    if #args == 0 or help_requested(args) then
        return jagen.command['help']({ args[2] })
    elseif jagen.command[cmd] then
        table.remove(args, 1)
        return jagen.command[cmd](args)
    else
        jagen.die("invalid command or argument '%s', try 'jagen help'", cmd)
    end
end

os.exit(jagen:run(arg) or 0)
