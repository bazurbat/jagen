
Target = {}
Target.__index = Target

function Target:new(name, stage, config)
    local target = {
        name   = name,
        stage  = stage,
        config = config,
        inputs = {}
    }
    setmetatable(target, self)
    return target
end

function Target:from_rule(rule, name, config)
    local stage = rule[1]; assert(type(stage) == 'string')
    local target = Target:new(name, stage, config)

    for i = 2, #rule do
        local input = rule[i]
        table.insert(target.inputs, Target:new(input[1], input[2], input[3]))
    end

    return target
end

function Target:from_arg(arg)
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

    return Target:new(name, stage, config)
end

function Target:__eq(other)
    return self.name   == other.name   and
           self.stage  == other.stage  and
           self.config == other.config
end

function Target:__tostring(sep)
    local o = {}
    sep = sep or '__'
    if self.name   then table.insert(o, self.name)   end
    if self.stage  then table.insert(o, self.stage)  end
    if self.config then table.insert(o, self.config) end
    return table.concat(o, sep)
end

function Target:add_inputs(target)
    for _, i in ipairs(target.inputs) do
        local function eq(t)
            return t == i
        end
        local found = find(eq, self.inputs)
        if not found then
            table.insert(self.inputs, i)
        end
    end

    return self
end

--}}}
