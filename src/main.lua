require 'common'
require 'Source'
require 'Ninja'

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

function jagen.show_help(section)
    section = section or 'usage'
    local help = require 'help'

    if help[section] then
        io.write(help[section])
    else
        jagen.error('no such help section: %s', section)
        return 2
    end
end

-- these should return status number or nothing (nil)

jagen.command = {}

function jagen.command.clean(args, i)
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

    return jagen.command.refresh()
end

function jagen.command.update()
    local source = GitSource:new({
            path = jagen.dir
        })
    if source:dirty() then
        jagen.die('%s is dirty, not updating', jagen.dir)
    else
        assert(source:exec('pull', '--ff-only'))
    end
    return jagen.command.refresh()
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

function jagen.command.refresh()
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

function jagen.command.run(options, rest)
    local packages = rules.merge(rules.load())

    for _, arg in ipairs(rest) do
        for _, target in ipairs(find_targets(packages, arg)) do
            table.insert(options, target)
        end
    end

    local err, status = system.exec(jagen.cmd, 'run', unpack(options))
    return status
end

function jagen.command.src(options, rest)
    local src = SourceManager:new()
    local cmd = rest[1]

    if not cmd then
        jagen.die("subcommand required, try 'jagen src help'")
    elseif src[cmd..'_command'] then
        return src[cmd..'_command'](src, table.rest(rest, 2))
    else
        jagen.die("'%s' is not valid src subcommand, use 'jagen src help'", cmd)
    end
end

function jagen.parse_args(args)
    local cmd, options, rest = nil, {}, {}
    local function is_option(arg)
        return string.sub(arg, 1, 1) == '-'
    end
    for i = 1, #args do
        local arg = args[i]
        if is_option(arg) then
            table.insert(options, arg)
        elseif cmd then
            table.insert(rest, arg)
        else
            cmd = arg
        end
    end
    return cmd, options, rest
end

function jagen:run(args)
    local cmd, options, rest = self.parse_args(args)
    local first = options[1]

    --[[ Handling the following cases:
    --   jagen
    --   jagen --help [cmd] 
    --   jagen help [cmd]
    --   jagen <cmd> help
    --   jagen <cmd> --help (effectively the second case) ]]

    if not cmd then
        return self.show_help('usage')
    elseif first == '-h' or first == '--help' then
        return self.show_help(cmd)
    elseif cmd == 'help' then
        return self.show_help(rest[1])
    elseif cmd and rest[1] == 'help' then
        return self.show_help(cmd)
    elseif jagen.command[cmd] then
        return jagen.command[cmd](options, rest)
    else
        jagen.die("invalid command '%s', try 'jagen help'", cmd)
    end
end

os.exit(jagen:run(arg) or 0)
