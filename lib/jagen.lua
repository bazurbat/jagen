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
    local list = {}
    for _, arg in ipairs({...}) do
        for _, i in ipairs(arg) do
            table.insert(list, i)
        end
    end
    return list
end

function for_each(f, t)
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

function find_last(f, l)
    local prev
    for _, v in ipairs(l or {}) do
        if not f(v) then
            return prev
        end
        prev = v
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

function Target.new(p, n, c)
    local t = { package = p, name = n, config = c }
    setmetatable(t, Target.meta)
    return t
end

Target.meta.__eq = function(a, b)
    return a.package == b.package and
    a.name == b.name and
    a.config == b.config
end

Target.meta.__tostring = function(t)
    local pkg_name = t.package.name
    if t.config then
        return string.format("%s-%s-%s", pkg_name, t.name, t.config)
    else
        return string.format("%s-%s", pkg_name, t.name)
    end
end

function load_package(rule)
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

    local function process_package(rule)
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

        local function process_stage(rule)
            local name, config

            if type(rule[1]) == 'string' then
                name = rule[1]
                table.remove(rule, 1)
            end
            if type(rule[1]) == 'string' then
                config = rule[1]
                table.remove(rule, 1)
            end

            local key = getkey(name, config)
            
            if tmp[key] then
                tmp[key].deps = append(tmp[key].deps or {}, list(rule))
            else
                local target = Target.new(package, name, config)
                target.deps = list(rule)
                tmp[key] = target
                table.insert(collected, target)
            end
        end

        for_each(process_stage, rule.stages)

        package.name = rule.name
        package.source = rule.source
        package.stages = collected

        print("===", package.name)
        pretty.dump(package)

        return package
    end

    local packages = map(process_package, rules)

    return packages
end

function Jagen.merge_stages(p, a, b)
    local t = {}
    local stages = {}

    local function getkey(s)
        if s.config then
            return s.name .. ':' .. s.config
        else
            return s.name
        end
    end

    -- print("== " .. p.name  .. " ==")

    local function collect(list)
        for _, s in ipairs(list or {}) do
            local key = getkey(s)
            if t[key] then
                for _, d in ipairs(s) do
                    table.insert(t[key], d)
                end
            else
                t[key] = s
                table.insert(stages, s)
            end
        end
    end

    collect(a)
    collect(b)

    local last, last_common

    local function common_stage(t)
        return t.name and not t.config
    end

    -- for _, s in ipairs(stages or {}) do
    --     local key = getkey(s)
    --     if last then
    --         local dep = Target.new(p.name, last.name, last.config)
    --         local function equals(t)
    --             return t == dep
    --         end
    --         if not find(equals, t[key]) then
    --             table.insert(t[key], 1, dep)
    --         end
    --         if s.config ~= last.config then
    --             last_common = find_last(common_stage, stages)
    --             last = last_common
    --
    --         else
    --             last = s
    --         end
    --     else
    --         last = s
    --     end
    -- end

    return stages
end

function Ninja:format_dependencies(pkg, stage)
    local t = {}
    for _, d in ipairs(stage) do
        table.insert(t, tostring(d))
    end
    return t
end

function Ninja:generate(out_file, in_file)
    local packages = load_rules(in_file)
    local out = io.open(out_file, 'w')

    -- out:write(string.format('builddir = %s\n\n', 'build'))
    -- out:write(string.format('rule command\n'))
    -- out:write(string.format('    command = $command\n\n'))
    -- out:write(string.format('rule script\n'))
    -- out:write(string.format('    command = $script\n\n'))
    --
    -- local sep = string.format(' $\n%s', Jagen.format_indent(16))
    --
    -- for i, pkg in ipairs(packages) do
    --     local pn = pkg.name
    --     for j, stage in ipairs(pkg.stages or {}) do
    --         local sn = stage.name
    --         local sc = stage.config
    --         out:write(string.format('build %s: script',
    --             tostring(Target.new(pkg, sn, sc))))
    --         if #stage > 0 then
    --             out:write(' $\n' .. Jagen.format_indent(16))
    --             out:write(table.concat(Ninja:format_dependencies(pkg, stage), sep))
    --         end
    --         out:write('\n')
    --         out:write(string.format('    script = jagen-pkg %s %s', pn, sn))
    --         if sc then
    --             out:write(' ', sc)
    --         end
    --         out:write('\n')
    --     end
    --     out:write("\n")
    -- end

    out:close()
end

function pkg_flag(f)
    return true
end

function env(v)
    return v
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
