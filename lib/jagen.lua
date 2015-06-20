--{{{ common

function copy(t)
    local c = {}
    for k, v in pairs(t) do
        if type(v) == 'table' then
            v = copy(v)
        end
        c[k] = v
    end
    return c
end

function list(t)
    local o = {}
    for _, v in ipairs(t or {}) do
        table.insert(o, v)
    end
    return o
end

function append(...)
    local o = {}
    for _, arg in ipairs({...}) do
        for _, i in ipairs(arg) do
            table.insert(o, i)
        end
    end
    return o
end

function for_each(t, f)
    for _, v in ipairs(t or {}) do
        f(v)
    end
end

function map(f, t)
    local r = {}
    for i, v in ipairs(t or {}) do
        table.insert(r, f(v))
    end
    return r
end

function find(f, t)
    for _, v in ipairs(t or {}) do
        if f(v) then
            return v
        end
    end
    return nil
end

function filter(pred, list)
    local o = {}
    for _, v in ipairs(list or {}) do
        if pred(v) then
            table.insert(o, v)
        end
    end
    return o
end

function compose(f, g)
    return function (...)
        f(unpack(g(...)))
    end
end

local function find_by_name(name, list)
    local function by_name(x)
        return x.name == name
    end
    return find(by_name, list)
end

function string.split(s, sep)
    local o, b, e = {}
    local init = 1

    repeat
        b, e = string.find(s, sep, init, true)
        if not b then b = 0 end
        table.insert(o, string.sub(s, init, b-1))
        if e then init = e + 1 end
    until b == 0

    return o
end

function table.rest(t, start)
    local o = {}
    for i = start, #t do
        table.insert(o, t[i])
    end
    return o
end

--}}}
--{{{ system

local system = {}

function system.mkpath(...)
    local sep = '/'
    local path = {}
    for _, c in ipairs({...}) do
        table.insert(path, c)
    end
    return table.concat(path, sep)
end

function system.mkdir(pathname)
    system.exec('mkdir -p "' .. pathname .. '"')
end

function system.file_newer(file1, file2)
    local cmd = string.format('[ "%s" -nt "%s" ]', file1, file2)
    return os.execute(cmd) == 0
end

function system.file_older(file1, file2)
    local cmd = string.format('[ "%s" -ot "%s" ]', file1, file2)
    return os.execute(cmd) == 0
end

function system.exec(command, ...)
    local cmd = { command }
    for _, arg in ipairs({...}) do
        table.insert(cmd, string.format('%q', tostring(arg)))
    end
    local line = table.concat(cmd, ' ')
    jagen.debug1(line)
    local status = os.execute(line)
    return status
end

--}}}
--{{{ format

local format = {}

function format.indent(n)
    local t = {}
    for i = 1, n do
        table.insert(t, " ")
    end
    return table.concat(t)
end

--}}}
--{{{ ninja

ninja = {}

function ninja:format_inputs(inputs)
    local sep = string.format(' $\n%s', format.indent(16))
    local t = {}
    for _, d in ipairs(inputs) do
        table.insert(t, tostring(d))
    end
    return table.concat(t, sep)
end

function ninja:generate(packages, out_file)
    local out = io.open(out_file, 'w')

    out:write(string.format('builddir = %s\n\n', os.getenv('pkg_build_dir')))
    out:write(string.format('rule command\n'))
    out:write(string.format('    command = $command\n\n'))
    out:write(string.format('rule script\n'))
    out:write(string.format('    command = ' .. os.getenv('pkg_bin_dir') .. '/$script && touch $out\n\n'))

    local sep = string.format(' $\n%s', format.indent(16))

    for i, pkg in ipairs(packages) do
        local pn = pkg.name
        for j, stage in ipairs(pkg.stages or {}) do
            local sn = stage.stage
            local sc = stage.config
            out:write(string.format('build %s: script', tostring(stage)))
            if #stage.inputs > 0 then
                out:write(' $\n' .. format.indent(16))
                out:write(ninja:format_inputs(stage.inputs))
            end
            out:write('\n')
            out:write(string.format('    script = jagen-pkg %s %s', pn, sn))
            if sc then
                out:write(' ', sc)
            end
            out:write('\n')
        end
        out:write("\n")
    end

    out:close()
end

--}}}
--{{{ target

target = { meta = {} }

function target.new(n, s, c)
    local t = { name = n, stage = s, config = c }
    setmetatable(t, target.meta)
    return t
end

function target.new_from_arg(arg)
    local name, stage, config
    local c = string.split(arg, ':')

    if c[1] and #c[1] > 0 then
        name = c[1]
    end
    if c[2] and #c[2] > 0 then
        stage = c[2]
    end
    if c[3] and #c[3] > 0 then
        config = c[3]
    end

    return target.new(name, stage, config)
end

function target.maybe_add_stage(t, stage)
    if not t.stage then
        t.stage = stage
    end
    return t
end

target.meta.__eq = function(a, b)
    return a.name == b.name and
    a.stage == b.stage and
    a.config == b.config
end

target.meta.__tostring = function(t)
    return table.concat({ t.name, t.stage, t.config }, '-')
end

--}}}
--{{{ jagen

jagen =
{
    debug = os.getenv('pkg_debug'),
    flags = os.getenv('pkg_flags'),
    sdk   = os.getenv('pkg_sdk'),

    bin_dir   = os.getenv('pkg_bin_dir'),
    lib_dir   = os.getenv('pkg_lib_dir'),
    src_dir   = os.getenv('pkg_src_dir'),
    build_dir = os.getenv('pkg_build_dir'),

    patch_dir         = os.getenv('pkg_patch_dir'),
    build_include_dir = os.getenv('pkg_build_include_dir'),
    private_dir       = os.getenv('pkg_private_dir'),
}

jagen.pkg_dir = system.mkpath(jagen.lib_dir, 'pkg')

jagen.cmd = system.mkpath(jagen.lib_dir, 'cmd.sh')
jagen.rules_file = system.mkpath(jagen.lib_dir, 'rules.'..jagen.sdk..'.lua')
jagen.build_file = system.mkpath(jagen.build_dir, 'build.ninja')

function jagen.exec(...)
    return system.exec(jagen.cmd, ...)
end

function jagen.tostring(...)
    return table.concat(map(tostring, {...}), ' ')
end

function jagen.message(...)
    print(string.format('\027[1;34m:::\027[0m %s', jagen.tostring(...)))
end
function jagen.warning(...)
    print(string.format('\027[1;33m:::\027[0m %s', jagen.tostring(...)))
end
function jagen.error(...)
    print(string.format('\027[1;31m:::\027[0m %s', jagen.tostring(...)))
end

function jagen.debug(...)
    if jagen.debug then
        print(string.format('\027[1;36m:::\027[0m %s', jagen.tostring(...)))
    end
end
function jagen.debug1(...)
    if os.getenv('pkg_debug') >= '1' then
        print(string.format('\027[1;36m:::\027[0m %s', jagen.tostring(...)))
    end
end
function jagen.debug2(...)
    if os.getenv('pkg_debug') >= '2' then
        print(string.format('\027[1;36m:::\027[0m %s', jagen.tostring(...)))
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

function jagen.convert_stages(rule)
    local stages = {}
    for i, s in ipairs(rule) do
        table.insert(stages, s)
        rule[i] = nil
    end
    rule.stages = stages
    return rule
end

function jagen.merge(a, b)
    for k, v in pairs(b) do
        if type(k) ~= 'number' then
            if type(v) == 'table' then
                a[k] = jagen.merge(a[k] or {}, v)
            else
                a[k] = v
            end
        end
    end
    for _, v in ipairs(b) do
        table.insert(a, v)
    end
    return a
end

function jagen.read_pkg(name)
    local path = system.mkpath(jagen.pkg_dir, name..'.lua')
    local env = {}
    local o = {}

    function env.package(rule)
        o = rule
    end

    local def = loadfile(path)
    if def then
        setfenv(def, env)
        def()
    end

    return o
end

function jagen.read(rule, stages)
    local default_stages = {
        { 'clean' }, { 'unpack' }, { 'patch' }
    }

    local pkg_rule = jagen.read_pkg(rule.name)

    pkg_rule = jagen.convert_stages(jagen.merge(pkg_rule, rule))
    pkg_rule.stages = append(default_stages, stages or {}, pkg_rule.stages)

    return pkg_rule
end

function jagen.load_rules()
    assert(jagen.sdk)
    local rules = dofile(jagen.rules_file)

    local function load_package(pkg_rule)
        local package = {}
        local tmp = {}
        local collected = {}

        local function load_source(source)
            if type(source) == 'string' then
                return { type = 'dist', location = source }
            else
                return source
            end
        end

        local function getkey(name, config)
            if config then
                return name .. ':' .. config
            else
                return name
            end
        end

        local function input_to_target(d)
            return target.new(d[1], d[2], d[3])
        end

        local function load_stage(stage_rule)
            local stage, config

            if type(stage_rule[1]) == 'string' then
                stage = stage_rule[1]
                table.remove(stage_rule, 1)
            end
            if type(stage_rule[1]) == 'string' then
                config = stage_rule[1]
                table.remove(stage_rule, 1)
            end

            local key = getkey(stage, config)
            local inputs = map(input_to_target, list(stage_rule))

            if tmp[key] then
                tmp[key].inputs = append(tmp[key].inputs or {}, inputs)
            else
                local target = target.new(pkg_rule.name, stage, config)
                target.inputs = inputs
                tmp[key] = target
                table.insert(collected, target)
            end
        end

        function add_previous(stages)
            local prev, common

            for _, s in ipairs(stages) do
                if prev then
                    if common and s.config ~= prev.config then
                        table.insert(s.inputs, 1, common)
                    else
                        table.insert(s.inputs, 1, prev)
                    end
                end

                prev = s
                if not s.config then
                    common = s
                end
            end
        end

        for_each(pkg_rule.stages, load_stage)
        add_previous(collected)

        package.name = pkg_rule.name
        package.source = load_source(pkg_rule.source)
        package.patches = pkg_rule.patches
        package.config = pkg_rule.config
        package.stages = collected

        return package
    end

    local packages = map(load_package, rules)

    for _, pkg in ipairs(packages) do
        packages[pkg.name] = pkg
    end

    return packages
end

function jagen.generate_include_script(pkg)
    local name = pkg.name
    local filename = system.mkpath(jagen.build_include_dir, name .. '.sh')

    local function source(pkg)
        local source = pkg.source
        local o, s = {}, {}
        if source.type == 'git' or source.type == 'hg' then
            table.insert(s, source.type)
            table.insert(s, source.location)
        elseif source.type == 'dist' then
            table.insert(s, system.mkpath('$pkg_dist_dir', source.location))
        end
        table.insert(o, string.format('p_source="%s"', table.concat(s, ' ')))
        if source.branch then
            table.insert(o, string.format('p_source_branch="%s"', source.branch))
        end
        if source.directory then
            table.insert(o, string.format('p_source_dir="%s"', source.directory))
        end
        return table.concat(o, '\n')
    end

    local function patches(pkg)
        local o = {}
        table.insert(o, '\npkg_patch_pre() {')
        for _, patch in ipairs(pkg.patches or {}) do
            local name = patch[1]
            local strip = patch[2]
            table.insert(o, string.format('  p_patch %d "%s"', strip, name))
        end
        table.insert(o, '}')
        return table.concat(o, '\n')
    end

    local function config(pkg)
        local o = {}
        local config = pkg.config
        table.insert(o, '\nuse_toolchain '..config)
        table.insert(o, 'p_system="$target_system"')
        table.insert(o, 'p_prefix="$target_prefix"')
        table.insert(o, 'p_dest_dir="$target_dir"')
        return table.concat(o, '\n')
    end

    local f = assert(io.open(filename, 'w+'))
    f:write('#!/bin/sh\n')
    if pkg.source then
        f:write(source(pkg))
    end
    if pkg.patches then
        f:write(patches(pkg))
    end
    if pkg.config then
        f:write(config(pkg))
    end
    f:close()
end

function jagen.generate()
    local packages = jagen.load_rules()
    ninja:generate(packages, jagen.build_file)
    for_each(packages, jagen.generate_include_script)
end

--}}}
--{{{ pkg

local pkg = {}

function pkg.filter(pkg, target)
    local function match_config(a, b)
        return not a.config or a.config == b.config
    end
    local function match_stage(a, b)
        return not a.stage or a.stage == b.stage
    end
    local function match_target(stage)
        return match_stage(target, stage) and match_config(target, stage)
    end
    return pkg and filter(match_target, pkg.stages) or {}
end

function pkg.type(p)
    local source = p.source
    return source and source.type
end

function pkg.is_source(p)
    local src = pkg.type(p)
    return src == 'git' or src == 'hg'
end

function pkg.directory(p)
    local function basename(location)
        return location and io.popen('basename '..location..' .git'):read()
    end
    if pkg.is_source(p) then
        local location = p.source.location
        local name = p.source.directory or basename(location)
        return system.mkpath(jagen.src_dir, name or p.name)
    else
        return system.mkpath(jagen.build_dir, 'pkg', p.name)
    end
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

    if is_param(arg) then
        table.insert(args, arg)
    else
        local target = target.new_from_arg(arg)
        targets = pkg.filter(packages[target.name], target)
        if #targets == 0 then
            jagen.warning('No targets found for:', arg)
        end
    end

    return targets, args
end

function jagen.build(args)
    local packages = jagen.load_rules()
    local targets = {}

    for _, arg in ipairs(args) do
        targets = append(targets, build.find_targets(packages, arg))
    end

    return system.exec(jagen.cmd, 'build', unpack(targets))
end

function jagen.rebuild(args)
    local packages = jagen.load_rules()
    local targets = {}

    for _, arg in ipairs(args) do
        targets = append(targets, build.find_targets(packages, arg))
    end

    return system.exec(jagen.cmd, 'rebuild', unpack(targets))
end

---}}}
--{{{ src

local src = {}

function src.exec_git(p, ...)
    local dir = pkg.directory(p)
    return system.exec('git', '-C', dir, ...)
end

function src.popen_git(p, ...)
    local dir = pkg.directory(p)
    return io.popen('git -C '..dir..' '..jagen.tostring(...)):read() or ''
end

function src.exec_hg(p, ...)
    local dir = pkg.directory(p)
    return system.exec('hg', '-R', dir, ...)
end

function src.popen_hg(p, ...)
    local dir = pkg.directory(p)
    return io.popen('hg -R '..dir..' '..jagen.tostring(...)):read() or ''
end

function src.head(p)
    local kind = pkg.type(p)
    if kind == 'git' then
        return src.popen_git(p, 'rev-parse', 'HEAD')
    elseif kind == 'hg' then
        return src.popen_hg(p, 'id', '-i')
    end
end

function src.dirty(p)
    local kind = pkg.type(p)
    if kind == 'git' then
        return string.len(src.popen_git(p, 'status', '--porcelain')) > 0
    elseif kind == 'hg' then
        return string.len(src.popen_hg(p, 'status')) > 0
    end
end

function src.status(args)
    local packages = jagen.load_rules()
    local source_packages = filter(pkg.is_source, packages)

    for _, p in ipairs(source_packages) do
        local dirty = src.dirty(p) and 'dirty' or ''
        print(string.format("%s: %s %s", p.name, src.head(p), dirty))
    end
end

--}}}

command = arg[1]
status = 0

if command == 'refresh' then
    jagen.generate()
elseif command == 'build' then
    local args = table.rest(arg, 2)

    status = jagen.build(args)
elseif command == 'rebuild' then
    local args = table.rest(arg, 2)

    status = jagen.rebuild(args)
elseif command == 'src' then
    local subcommand = arg[2]
    local args = table.rest(arg, 3)

    if not subcommand then
        jagen.message('Available src subcommands: status')
    elseif subcommand == 'status' then
        src.status(args)
    else
        jagen.die('Unknown src subcommand:', subcommand);
    end
else
    jagen.die('Unknown command:', command)
end

os.exit(status % 0xFF)
