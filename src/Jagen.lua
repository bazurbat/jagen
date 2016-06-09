require 'common'
require 'Ninja'

local System  = require 'System'
local Package = require 'Package'
local Target  = require 'Target'
local Source  = require 'Source'
local Log     = require 'Log'

local function die(...)
    Log.error(...)
    os.exit(1)
end

Jagen =
{
    dir  = os.getenv('jagen_dir'),
    root = os.getenv('jagen_root'),

    shell = os.getenv('jagen_shell'),

    flags = os.getenv('jagen_flags'),

    lib_dir     = os.getenv('jagen_lib_dir'),
    src_dir     = os.getenv('jagen_src_dir'),
    build_dir   = os.getenv('jagen_build_dir'),
    include_dir = os.getenv('jagen_include_dir'),
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

Jagen.src = {}

-- Should return 0 if true, 1 if false, for shell scripting.
function Jagen.src.dirty(packages)
    for _, pkg in ipairs(packages) do
        local source = pkg.source
        if System.exists(source.dir) and source:dirty() then
            return 0
        end
    end
    return 1
end

function Jagen.src.status(packages)
    for _, pkg in ipairs(packages) do
        local source = pkg.source
        if System.exists(source.dir) then
            local dirty = source:dirty() and 'dirty' or ''
            local head = source:head()
            if not head then
                die('failed to get source head for %s in %s',
                    pkg.name, source.dir)
            end
            print(string.format("%s (%s): %s %s", pkg.name, source.location, head, dirty))
        else
            print(string.format("%s (%s): not exists", pkg.name, source.location))
        end
    end
end

function Jagen.src.clean(packages)
    for _, pkg in ipairs(packages) do
        local source = pkg.source
        Log.message('clean %s in %s', pkg.name, source.dir)
        if not pkg.source:clean() then
            die('failed to clean %s (%s) in %s',
                pkg.name, source.branch, source.dir)
        end
    end
end

function Jagen.src.update(packages)
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
        if System.exists(source.dir) then
            old_head = source:head()
            if not source:dirty() then
                if offline then
                    Log.message('switch %s to %s in %s',
                        pkg.name, source.branch, source.dir)
                else
                    Log.message('update %s from %s to %s in %s',
                        pkg.name, source.location, source.branch, source.dir)
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
                Log.warning("skip update of %s because working directory '%s' is dirty",
                    pkg.name, source.dir)
            end
        else
            if offline then
                die("could not clone '%s' in offline mode", pkg.name)
            elseif not source:clone() then
                die('failed to clone %s from %s to %s',
                    pkg.name, source.location, source.dir)
            end
        end

        if not offline and not source:fixup() then
            die('failed to fix up %s source in %s',
                pkg.name, source.dir)
        end

        if source:head() ~= old_head then
            assert(Target:new(pkg.name, 'unpack'):touch())
        end
    end
end

function Jagen.src.delete(packages)
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

-- image

Jagen.image = {}

function Jagen.image.create(image_type)
    local Image = require 'Image'

    if image_type == 'rootfs' then
        local target_dir = assert(os.getenv('jagen_target_dir'))
        local out_file = '$jagen_build_dir/rootfs.ext4'

        Image:create(target_dir, out_file)
    elseif not image_type then
        die('image type is not specified')
    else
        die('unsupported image type: %s', image_type)
    end
end

-- these should return status number or nothing (nil)

Jagen.command = {}

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
    if help_requested(args) then
        return Jagen.command['help'] { 'clean' }
    end

    if #args > 0 then
        local packages = Package.load_rules()
        for _, arg in ipairs(args) do
            local match = string.gmatch(arg, '[^:]+')
            local name, config = match(), match()
            local pkg = packages[name]
            if not pkg then
                die('no such package: %s', name)
            end
            for config, dir in pairs(pkg:build_dirs(config)) do
                assert(System.rmrf(dir))
                assert(Target:new(name, 'configure', config):remove())
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
    local dirs = System.getenv(vars)

    assert(System.rmrf(unpack(dirs)))

    return Jagen.command.refresh()
end

local function prepare_root()
    local vars = {
        'jagen_bin_dir',
        'jagen_build_dir',
        'jagen_include_dir',
        'jagen_log_dir'
    }
    local dirs = System.getenv(vars)

    assert(System.mkdir(unpack(dirs)))
end

function Jagen.command.refresh(args)
    if help_requested(args) then
        return Jagen.command['help'] { 'refresh' }
    end

    prepare_root()

    local packages = Package.load_rules(true)
    local Script = require 'Script'
    local include_dir = assert(os.getenv('jagen_include_dir'))
    local log_dir = assert(os.getenv('jagen_log_dir'))

    for _, pkg in pairs(packages) do
        Script:write(pkg, include_dir)

        -- create/truncate all log files beforehand to allow tail following
        -- them on interactive rebuild
        for stage in pkg:each() do
            local filename = string.format('%s/%s.log', log_dir, tostring(stage))
            assert(io.open(filename, 'w+')):close()
        end
    end

    local ninja = Ninja:new()
    local build_file = System.mkpath(Jagen.build_dir, 'build.ninja')
    ninja:generate(build_file, packages)
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
    local options, rest = find_options(args)

    for _, arg in ipairs(rest) do
        for _, target in ipairs(find_targets(packages, arg)) do
            table.insert(options, target)
        end
    end

    local cmd = System.mkpath(Jagen.lib_dir, 'cmd.sh')
    local err, status = System.exec('%s build %s', cmd,
        System.quote(unpack(options)))

    return status
end

function Jagen.command.status()
    return Jagen.command.src({}, { 'status' })
end

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

function Jagen.command.src(args)
    if help_requested(args) then
        return Jagen.command['help'] { 'src' }
    end

    local options, rest = find_options(args)
    local command = rest[1]
    table.remove(rest, 1)

    if not command then
        die("command required, try 'jagen src help'")
    elseif Jagen.src[command] then
        local packages = scm_packages(rest)
        return Jagen.src[command](packages)
    else
        die("'%s' is not valid src command, use 'Jagen src help'", command)
    end
end

function Jagen.command.image(options, rest)
    local command = rest[1]

    if not command then
        die("command required, try 'jagen image help'")
    elseif Jagen.image[command] then
        return Jagen.image[command](rest[2])
    else
        die("'%s' is not valid image command, use 'Jagen image help'", command)
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
