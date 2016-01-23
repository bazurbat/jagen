require 'common'
require 'Source'
require 'Ninja'
require 'Script'

local system = require 'system'
local rules = require 'rules'
local SourceManager = require 'SourceManager'

jagen =
{
    dir  = os.getenv('jagen_dir'),
    root = os.getenv('jagen_root'),

    overlays = os.getenv('jagen_overlays'),

    shell = os.getenv('jagen_shell'),

    debug = os.getenv('jagen_debug'),
    flags = os.getenv('jagen_flags'),

    lib_dir     = os.getenv('jagen_lib_dir'),
    src_dir     = os.getenv('jagen_src_dir'),
    build_dir   = os.getenv('jagen_build_dir'),
    include_dir = os.getenv('jagen_include_dir'),

    patch_dir   = os.getenv('jagen_patch_dir'),
    private_dir = os.getenv('jagen_private_dir'),

    nproc = assert(tonumber(io.popen('nproc'):read()))
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

local command = {}

function command.clean()
    local vars = {
        'jagen_build_dir',
        'jagen_include_dir',
        'jagen_log_dir',
        'jagen_host_dir',
        'jagen_target_dir',
        'jagen_tools_dir'
    }
    local dirs = system.getenv(vars)

    assert(system.rmrf(unpack(dirs)))

    return command.refresh()
end

function command.update()
    local source = GitSource:new({
            path = jagen.dir
        })
    if source:dirty() then
        jagen.die('%s is dirty, not updating', jagen.dir)
    else
        assert(source:exec('pull', '--ff-only'))
    end
    return command.refresh()
end

local function prepare_root()
    local vars = {
        'jagen_build_dir',
        'jagen_include_dir',
        'jagen_log_dir'
    }
    local dirs = system.getenv(vars)

    assert(system.mkdir(unpack(dirs)))
end

function command.refresh()
    prepare_root()

    local packages = rules.load()

    for _, rule in pairs(packages) do
        local filename = system.mkpath(jagen.include_dir, tostring(rule)..'.sh')
        local file = assert(io.open(filename, 'w+'))
        Script:write(rule, file)
        file:close()
    end

    packages = rules.merge(packages)

    for _, pkg in pairs(packages) do
        pkg:add_ordering_dependencies()

        local filename = system.mkpath(jagen.include_dir, pkg.name..'-shared.sh')
        local file = assert(io.open(filename, 'w+'))
        Script:write_shared(pkg, file)
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

    for _, pkg in ipairs(packages) do
        for _, stage in ipairs(pkg.stages) do
            if match_target(target, stage) then
                table.insert(targets, stage)
            end
        end
    end

    if #targets == 0 then
        jagen.die('could not find targets matching the argument: %s', arg)
    end

    return targets
end

function command.run(options, rest)
    local packages = rules.merge(rules.load())
    local args = options or {}

    for _, arg in ipairs(rest) do
        for _, tgt in ipairs(find_targets(packages, arg)) do
            table.insert(args, tgt)
        end
    end

    return system.exec(jagen.cmd, 'run', unpack(args))
end

function command.src(options, rest)
    local subcommand = rest[1]
    local args = table.rest(rest, 3)
    local src = SourceManager:new()

    if not subcommand then
        jagen.die('no src subcommand specified')
    end

    if src[subcommand..'_command'] then
        status = src[subcommand..'_command'](src, args)
    else
        jagen.die('unknown src subcommand: %s', subcommand)
    end
end

local function print_help(command)
    return system.exec(jagen.cmd, 'help', command)
end

local function is_option(arg)
    return string.sub(arg, 1, 1) == '-'
end

local function process_arguments(args)
    local options, rest, need_help, command = {}, {}, false

    for _, arg in ipairs(args) do
        if is_option(arg) then
            if arg == '-h' or arg == '--help' then
                need_help = true
            else
                table.insert(options, arg)
            end
        elseif command then
            table.insert(rest, arg)
        else
            command = arg
        end
    end

    if need_help then
        print_help(command)
        os.exit(0)
    end

    return command, options, rest
end

name, options, rest = process_arguments(arg)

if not name then
    print_help()
    os.exit(0)
elseif command[name] then
    command[name](options, rest)
else
    jagen.die('unknown command: %s', name)
end

os.exit((status or 0) % 0xFF)
