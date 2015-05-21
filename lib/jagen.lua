require 'pl'

Jagen = {}
Ninja = {}

function load_rules(pathname)
    return dofile(pathname)
end

function imap(f, t)
    local r = {}
    for i, v in ipairs(t or {}) do
        table.insert(r, f(v))
    end
    return r
end

function ifind(f, t)
    for _, v in ipairs(t or {}) do
        if f(v) then
            return true
        end
    end
    return false
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
    if t.config then
        return string.format("%s-%s-%s", t.name, t.stage, t.config)
    else
        return string.format("%s-%s", t.name, t.stage)
    end
end

Stage = { meta = {} }

function Stage.new(p, n, c)
    local stage = { package = p, name = n, config = c }
    setmetatable(stage, Stage.meta)
    return stage
end

Stage.meta.__eq = function(a, b)
    print("Stage.eq")
    return a.package == b.package and
    a.name == b.name and
    a.config == b.config
end

Stage.meta.__tostring = function(s)
    if s.config then
        return string.format("%s: %s %s", s.package.name, s.name, s.config)
    else
        return string.format("%s: %s", s.package.name, s.name)
    end
end

function Jagen.merge_stages(p, a, b)
    local t = {}
    local stages = {}
    local prev = nil

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

    for _, s in ipairs(stages or {}) do
        local key = getkey(s)
        -- if prev then
        --     print("--- " .. prev.package.name)
        -- else
        --     print("---")
        -- end
        -- pretty.dump(s)
        if prev then
            local target = Target.new(p.name, prev.name, prev.config)
            local function eq(tgt)
                return target == tgt
            end
            if not ifind(eq, t[key]) then
                table.insert(t[key], 1, target)
            end
        end
        prev = s
    end

    return stages
end

function Jagen.package(p, stages)
    local function tostage(t)
        t.package = p
        if type(t[1]) == 'string' then
            t.name = t[1]
            table.remove(t, 1)
        end
        if type(t[1]) == 'string' then
            t.config = t[1]
            table.remove(t, 1)
        end
        for i, d in ipairs(t) do
            t[i] = Target.new(d[1], d[2], d[3])
        end
        setmetatable(t, Stage.meta)
        return t
    end

    local default_stages = imap(tostage, {
            { 'update' }, { 'clean' }, { 'unpack' }, { 'patch' }
        })
    local more_stages = imap(tostage, stages)

    imap(tostage, p)

    local s1 = Jagen.merge_stages(p, default_stages, more_stages)
    s1 = Jagen.merge_stages(p, s1, p)

    p.stages = s1

    return p
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

    out:write(string.format('builddir = %s\n\n', 'build'))
    out:write(string.format('rule command\n'))
    out:write(string.format('    command = $command\n\n'))
    out:write(string.format('rule script\n'))
    out:write(string.format('    command = $script\n\n'))

    local sep = string.format(' $\n%s', Jagen.format_indent(16))

    for i, pkg in ipairs(packages) do
        local pn = pkg.name
        for j, stage in ipairs(pkg.stages or {}) do
            local sn = stage.name
            local sc = stage.config
            -- print(pn, sn, sc)
            out:write(string.format('build %s: script',
                tostring(Target.new(pn, sn, stage.config))))
            if #stage > 0 then
                out:write(' $\n' .. Jagen.format_indent(16))
                out:write(table.concat(Ninja:format_dependencies(pkg, stage), sep))
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
