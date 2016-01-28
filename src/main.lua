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

function command.help(start)
    local help = require 'help'
    local section
    if not start then
        section = 'usage'
    elseif type(start) == 'string' then
        section = start
    elseif type(start) == 'number' then
        section = arg[start] or 'usage'
    end
    if help[section] then
        io.write(help[section])
    else
        jagen.die('no such help section: %s', section)
    end
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

function command.run(start)
    local options, args, need_help = {}, {}

    for i = start, #arg do
        if main.is_option(arg[i]) then
            if arg[i] == '-h' or arg[i] == '--help' then
                need_help = true
                break
            else
                table.insert(options, arg[i])
            end
        else
            if arg[i] == 'help' then
                need_help = true
            else
                table.insert(args, arg[i])
            end
        end
    end

    if need_help then
        return command.help('usage')
    end

    local packages = rules.merge(rules.load())

    for _, arg in ipairs(args) do
        for _, tgt in ipairs(find_targets(packages, arg)) do
            table.insert(options, tgt)
        end
    end

    return system.exec(jagen.cmd, 'run', unpack(options))
end

function command.src(start)
    local cmd, names = nil, {}

    for i = start, #arg do
        if main.is_option(arg[i]) then
            if arg[i] == '-h' or arg[i] == '--help' then
                cmd = 'help'
                break
            else
                jagen.die('invalid argument: %s', arg[i])
            end
        else
            if cmd then
                table.insert(names, arg[i])
            else
                cmd = arg[i]
            end
        end
    end

    local src = SourceManager:new()

    if not cmd then
        jagen.die("subcommand required, try 'jagen src help'")
    elseif cmd == 'help' then
        return command.help('src')
    elseif src[cmd..'_command'] then
        return src[cmd..'_command'](src, names)
    else
        jagen.die("'%s' is not valid src subcommand, use 'jagen src help'", cmd)
    end
end

function main.is_option(arg)
    return string.sub(arg, 1, 1) == '-'
end

local err, status

if #arg == 0 then
    command.help()
    os.exit(0)
end

for i, arg in ipairs(arg) do
    if main.is_option(arg) then
        if arg == '-h' or arg == '--help' then
            command.help()
            break
        else
            jagen.die('invalid argument: %s', arg)
        end
    else
        if command[arg] then
            err, status = command[arg](i + 1)
            break
        else
            jagen.die('invalid command: %s', arg)
        end
    end
end

os.exit((status or 0) % 0xFF)
