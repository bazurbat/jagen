require 'common'
require 'src'
require 'gen'

--{{{ jagen

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

function jagen.generate()
    local rules = Rules.load()
    local packages = {}

    for rule in each(rules) do
        local name = rule.name
        local pkg = packages[name]
        if pkg then
            for target in each(rule.stages) do
                pkg:add_target(target)
            end
        else
            packages[name] = rule
            table.insert(packages, rule)
        end

        local script = Script:new(rule)
        script:write()
    end

    for pkg in each(packages) do
        pkg:add_ordering_dependencies()
    end

    table.sort(packages, function (a, b)
            return a.name < b.name
        end)

    for _, pkg in ipairs(packages) do
        for _, stage in ipairs(pkg.stages) do
            table.sort(stage.inputs, function (a, b)
                    return tostring(a) < tostring(b)
                end)
        end
    end

    local ninja = Ninja:new()
    ninja:generate(jagen.build_file, packages)
end

--}}}
--{{{ build

local build = {}

function build.find_targets(packages, arg)
    local targets = {}
    local args = {}

    local function is_param(arg)
        return string.sub(arg, 1, 1) == '-'
    end
    local function match_config(a, b)
        return not a.config or a.config == b.config
    end
    local function match_stage(a, b)
        return not a.stage or a.stage == b.stage
    end
    local function match_target(target, stage)
        return match_stage(target, stage) and match_config(target, stage)
    end

    if is_param(arg) then
        table.insert(args, arg)
    else
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
            table.insert(args, arg)
        end
    end

    return targets, args
end

function jagen.build(args)
    local packages = Rules.load()
    local targets = {}

    for _, arg in ipairs(args) do
        targets = append(targets, build.find_targets(packages, arg))
    end

    return system.exec(jagen.cmd, 'build', unpack(targets))
end

function jagen.rebuild(args)
    local packages = Rules.load()
    local targets = {}

    for _, arg in ipairs(args) do
        targets = append(targets, build.find_targets(packages, arg))
    end

    return system.exec(jagen.cmd, 'run', unpack(targets))
end

---}}}

command = arg[1]
status = 0

if command == 'refresh' then
    jagen.generate()
elseif command == 'build' then
    local args = table.rest(arg, 2)

    _, status = jagen.build(args)
elseif command == 'run' then
    local args = table.rest(arg, 2)

    _, status = jagen.rebuild(args)
elseif command == 'src' then
    local subcommand = arg[2]
    local args = table.rest(arg, 3)
    local src = SourceManager:new()

    if not subcommand then
        jagen.die('no src subcommand specified')
    end

    if src[subcommand..'_command'] then
        status = src[subcommand..'_command'](src, args)
    else
        jagen.die('unknown src subcommand: %s', subcommand)
    end
else
    jagen.die('Unknown command: %s', command)
end

os.exit((status or 0) % 0xFF)
