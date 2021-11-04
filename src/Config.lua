local Rule = require 'Rule'

local Config = Rule:new()

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

return Config
