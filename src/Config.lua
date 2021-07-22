local Chunk = require 'Chunk'

local Config = Chunk:new()

function Config:new(rule)
    if rule.extends then
        local base = Config._all[rule.extends]
        rule.extends = nil
        rule._base = base
        rule._base_ = base
    else
        setmetatable(rule, self)
        self.__index = self
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
