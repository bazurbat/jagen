require 'pl'

Jagen = {}
Ninja = {}

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
        package.source = pkg_rule.source
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

function Ninja:generate(out_file, in_file)
    local packages = load_rules(in_file)
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

function pkg_flag(f)
    return true
end

function env(name)
    return os.getenv(name)
end

function Jagen.format_indent(n)
    local t = {}
    for i = 1, n do
        table.insert(t, " ")
    end
    return table.concat(t)
end

if arg[1] == 'generate' then
    Ninja:generate(arg[2], arg[3])
end
