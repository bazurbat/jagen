
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

