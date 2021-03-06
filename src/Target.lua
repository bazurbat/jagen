local Command = require 'Command'

local Target = {}
Target.__index = Target

function Target:new(o)
    o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Target.from_args(name, stage, config)
    return Target:new {
        name   = name,
        stage  = stage,
        config = config,
    }
end

function Target:parse(rule, name, config)
    local stage = rule[1]; assert(type(stage) == 'string')
    if type(rule[2]) == 'string' then
        config = rule[2]
        table.remove(rule, 2)
    end
    local target = Target.from_args(name, stage, config)

    for k, v in pairs(rule) do
        target[k] = rule[k]
    end

    if #rule > 1 then
        target.inputs = target.inputs or {}
    end

    for i = 2, #rule do
        local input = rule[i]
        append(target.inputs, Target.from_args(input[1], input[2], input[3]))
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

    return Target.from_args(name, stage, config)
end

function Target.from_use(spec)
    local function parse(spec)
        local spec, alias = unpack(spec:split(' as '))
        local name, config = unpack(string.split2(spec, ':'))
        local target = Target.from_args(name, nil, config)
        if not string.empty(alias) then
            target.alias = alias
        end
        return target
    end
    if type(spec) == 'string' then
        return parse(spec)
    elseif type(spec) == 'table' then
        local use = parse(spec[1])
        if spec.as then
            use.alias = spec.as
        elseif type(spec[2]) == 'string' then
            use.alias = spec[2]
        end
        return use
    else
        error('invalid use specification: '..pretty(spec))
    end
end

function Target:to_stage()
    if self.config then
        return string.format('%s:%s', self.stage, self.config)
    else
        return self.stage
    end
end

function Target:__lt(other)
    return self.name   < other.name   and
           self.stage  < other.stage  and
           self.config < other.config
end

function Target:__eq(other)
    return self.name   == other.name   and
           self.stage  == other.stage  and
           self.config == other.config
end

function Target:__tostring(sep)
    local o = {}
    sep = sep or ':'
    if self.name   then table.insert(o, self.name)   end
    if self.stage  then table.insert(o, self.stage)  end
    if self.config then table.insert(o, self.config) end
    return table.concat(o, sep)
end

function Target:__rawtostring()
    local saved = Target.__tostring
    Target.__tostring = nil
    local s = tostring(self)
    Target.__tostring = saved
    return s
end

function Target:add_inputs(target)
    assert(target)
    if target.inputs then
        for item in each(target.inputs) do
            self:append(item)
        end
    end
    return self
end

function Target:append(input)
    self.inputs = append_uniq(input, self.inputs)
    return self
end

function Target:append_uses(input)
    self.uses = append_uniq(input, self.uses)
    return self
end

function Target:match(pattern)
    return string.match(tostring(self), pattern)
end

function Target:touch()
    return Command:new('cd "$jagen_build_dir" && touch', quote(self)):exec()
end

function Target:remove()
    return Command:new('cd "$jagen_build_dir" && rm -f', quote(self)):exec()
end

return Target
