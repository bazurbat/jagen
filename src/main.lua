require 'common'
require 'Source'
require 'Ninja'

local system = require 'system'
local rules = require 'rules'
local SourceManager = require 'SourceManager'

local main = {}

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

function command.help(options, rest)
    local command = rest[1]
    return system.exec(jagen.cmd, 'help', command)
end

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
    local script = require 'script'

    for _, pkg in pairs(packages) do
        script:write(pkg)
    end

    packages = rules.merge(packages)

    for _, pkg in pairs(packages) do
        pkg:add_ordering_dependencies()
        script:write_shared(pkg)
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
        for _, stage in ipairs(pkg.stages) do
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

function command.run(options, rest)
    local packages = rules.merge(rules.load())
    local args = assert(options)

    for _, arg in ipairs(rest) do
        for _, tgt in ipairs(find_targets(packages, arg)) do
            table.insert(args, tgt)
        end
    end

    return system.exec(jagen.cmd, 'run', unpack(args))
end

function command.src(options, rest)
    local subcommand = rest[1]
    local args = table.rest(rest, 2)
    local src = SourceManager:new()

    if not subcommand then
        jagen.die('no src subcommand specified')
    end

    if src[subcommand..'_command'] then
        src[subcommand..'_command'](src, args)
    else
        jagen.die('unknown src subcommand: %s', subcommand)
    end
end

function main.is_option(arg)
    return string.sub(arg, 1, 1) == '-'
end

function main.process_common_arguments(args)
    for i, arg in ipairs(args) do
        if arg == '-h' or arg == '--help' then
            return 'help', 1
        elseif main.is_option(arg) then
            jagen.die('invalid argument: %s', arg)
        else
            return arg, i
        end
    end
end

function main.process_arguments(args, start)
    local options, rest = {}, {}
    for i = start, #args do
        if main.is_option(args[i]) then
            table.insert(options, args[i])
        else
            table.insert(rest, args[i])
        end
    end
    return options, rest
end

local name, index = main.process_common_arguments(arg)
local err, status

if not name then
    name = 'help'
    index = 1
end

if command[name] then
    err, status = command[name](main.process_arguments(arg, index + 1))
else
    jagen.die('invalid command: %s', name)
end

os.exit((status or 0) % 0xFF)
