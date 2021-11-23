local Chunk = require 'Chunk'
local Log   = require 'Log'

local Rule = Chunk:new()

function Rule:new(def)
    def = def or {}
    setmetatable(def, self)
    self.__index = self
    return def
end

function Rule.match(value, pattern, state)
    if type(pattern) == 'function' then
        return pattern(value, state)
    elseif type(value) ~= type(pattern) then
        return false
    elseif type(value) == 'table' then
        for k, v in pairs(pattern) do
            if not Rule.match(value[k], v, state) then
                return false
            end
        end
    elseif type(value) == 'string'
           and not string.match(value, pattern) then
        return false
    elseif type(value) ~= 'string' and value ~= pattern then
        return false
    end
    return true, state
end

function Rule.merge(to, from, state)
    for key, value in pairs(from or {}) do
        local tkey, tvalue = type(key), type(value)
        if tkey ~= 'number' then
            if tkey == 'function' then
                key = key(nil, state)
            end
            if tvalue == 'function' then
                value = value(nil, state)
            end
            if tvalue == 'table' then
                if type(to[key]) ~= 'table'  then
                    to[key] = {}
                end
                to[key] = Rule.merge(to[key], value, state)
            else
                to[key] = value
            end
        end
    end

    for i, value in ipairs(from or {}) do
        if type(value) == 'function' then
            value = value(nil, state)
        end
        if value ~= nil then
            if type(value) == 'table' then
                value = Rule.merge({}, value, state)
            end
            if value ~= nil then
                table.insert(to, value)
            end
        end
    end

    return to
end

return Rule
