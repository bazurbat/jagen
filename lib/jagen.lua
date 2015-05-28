-- require 'pl'

Jagen = {}
Ninja = {}

function Jagen.message(...)
    print(string.format('\027[1;34m:::\027[0m %s', table.concat({...}, ' ')))
end
function Jagen.warning(...)
    print(string.format('\027[1;33m:::\027[0m %s', table.concat({...}, ' ')))
end
function Jagen.error(...)
    print(string.format('\027[1;31m:::\027[0m %s', table.concat({...}, ' ')))
end
function Jagen.debug(...)
    if env('pkg_debug') == 'yes' then
        print(string.format('\027[1;36m:::\027[0m %s', table.concat({...}, ' ')))
    end
end

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
    local r = {}
    for _, v in ipairs(t or {}) do
        table.insert(r, v)
    end
    return r
end

function append(...)
    local r = {}
    for _, arg in ipairs({...}) do
        for _, i in ipairs(arg) do
            table.insert(r, i)
        end
    end
    return r
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

local function find_by_name(name, list)
    local function by_name(x)
        return x.name == name
    end
    return find(by_name, list)
end

Target = { meta = {} }

function Target.new(n, s, c)
    local t = { name = n, stage = s, config = c }
    setmetatable(t, Target.meta)
    return t
end

function Target.new_from_arg(arg)
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

    return Target.new(name, stage, config)
end

Target.meta.__eq = function(a, b)
    return a.name == b.name and
    a.stage == b.stage and
    a.config == b.config
end

Target.meta.__tostring = function(t)
    return table.concat({ t.name, t.stage, t.config }, '-')
end

function read_package(rule)
    local stages = {}
    for i, s in ipairs(rule) do
        table.insert(stages, s)
        rule[i] = nil
    end
    rule.stages = stages
    return rule
end

function load_rules(pathname)
    local rules = dofile(pathname)

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
            return Target.new(d[1], d[2], d[3])
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
                local target = Target.new(pkg_rule.name, stage, config)
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
        package.stages = collected

        -- print("===", package.name)
        -- pretty.dump(package)

        return package
    end

    local packages = map(load_package, rules)

    return packages
end

function Ninja:format_inputs(inputs)
    local sep = string.format(' $\n%s', Jagen.format_indent(16))
    local t = {}
    for _, d in ipairs(inputs) do
        table.insert(t, tostring(d))
    end
    return table.concat(t, sep)
end

function Ninja:generate(packages, out_file, in_file)
    local out = io.open(out_file, 'w')

    out:write(string.format('builddir = %s\n\n', env('pkg_build_dir')))
    out:write(string.format('rule command\n'))
    out:write(string.format('    command = $command\n\n'))
    out:write(string.format('rule script\n'))
    out:write(string.format('    command = ' .. env('pkg_bin_dir') .. '/$script && touch $out\n\n'))

    local sep = string.format(' $\n%s', Jagen.format_indent(16))

    for i, pkg in ipairs(packages) do
        local pn = pkg.name
        for j, stage in ipairs(pkg.stages or {}) do
            local sn = stage.stage
            local sc = stage.config
            out:write(string.format('build %s: script', tostring(stage)))
            if #stage.inputs > 0 then
                out:write(' $\n' .. Jagen.format_indent(16))
                out:write(Ninja:format_inputs(stage.inputs))
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

function Jagen.flag(f)
    return false
end

function env(name)
    return os.getenv(name)
end

local function exec(command, ...)
    local cmd = { command }
    for _, arg in ipairs({...}) do
        table.insert(cmd, string.format('%q', arg))
    end
    local status = os.execute(table.concat(cmd, ' '))
    return status
end

function Jagen.format_indent(n)
    local t = {}
    for i = 1, n do
        table.insert(t, " ")
    end
    return table.concat(t)
end

function Jagen.mkdir(pathname)
    exec('mkdir -p "' .. pathname .. '"')
end

function Jagen.file_newer(file1, file2)
    local cmd = string.format('[ "%s" -nt "%s" ]', file1, file2)
    return os.execute(cmd) == 0
end

function Jagen.file_older(file1, file2)
    local cmd = string.format('[ "%s" -ot "%s" ]', file1, file2)
    return os.execute(cmd) == 0
end

function Jagen.mkpath(...)
    local sep = '/'
    local path = {}
    for _, c in ipairs({...}) do
        table.insert(path, c)
    end
    return table.concat(path, sep)
end

function Jagen.generate_include_script(pkg)
    local name = pkg.name
    local dir = env('pkg_build_include_dir')
    local filename = Jagen.mkpath(dir, name .. '.sh')

    local function source(pkg)
        local o = {}
        local s = pkg.source
        if s then
            if s.type == 'git' or s.type == 'hg' then
                table.insert(o, s.type)
                table.insert(o, s.location)
            elseif s.type == 'dist' then
                table.insert(o, Jagen.mkpath('$pkg_dist_dir', s.location))
            end
        end
        return string.format('p_source="%s"\n', table.concat(o, ' '))
    end

    local function patches(pkg)
        local o = {}
        table.insert(o, 'pkg_patch_pre() {')
        for _, patch in ipairs(pkg.patches or {}) do
            local name = patch[1]
            local strip = patch[2]
            table.insert(o, string.format('  p_patch %d "%s"', strip, name))
        end
        table.insert(o, '}')
        return table.concat(o, '\n')
    end

    Jagen.mkdir(dir)

    local f = assert(io.open(filename, 'w+'))
    f:write('#!/bin/sh\n')
    f:write(source(pkg))
    if pkg.patches then
        f:write(patches(pkg))
    end
    f:close()
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

command = arg[1]

function Jagen.build(build_file, args)
    local targets = map(tostring, map(Target.new_from_arg, args))
    local build_command = Jagen.mkpath(env('pkg_lib_dir'), 'build.sh')

    return exec(build_command, unpack(targets))
end

function table.rest(t, start)
    local o = {}
    for i = start, #t do
        table.insert(o, t[i])
    end
    return o
end

if command == 'generate' then
    local build_file = arg[2]
    local rules_file = arg[3]

    if Jagen.file_older(build_file, rules_file) then
        Jagen.message("Generating build rules")
        local packages = load_rules(arg[3])
        Ninja:generate(packages, arg[2], arg[3])
        for_each(packages, Jagen.generate_include_script)
    end
elseif command == 'build' then
    local build_file = arg[2]
    local args = table.rest(arg, 3)

    return Jagen.build(build_file, args)
end
