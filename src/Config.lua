local Chunk = require 'Chunk'

local Config = Chunk:new()

function Config:new(rule)
    setmetatable(rule, self)
    self.__index = self
    return rule:_parse(rule)
end

function Config:_parse(rule)
    if type(rule[1]) == 'string' then
        self.name = rule[1]
        rule[1] = nil
    end
    return rule
end

function Config:flatten()
    local function import(to, from)
        for key, value in Config.each(from) do
            if rawget(to, key) == nil then
                to[key] = copy(value)
            elseif type(value) == 'table' then
                import(to[key], value)
            end
        end
    end
    local base = self._base
    while base ~= nil do
        import(self, base)
        base = base._base
    end
end

return Config
