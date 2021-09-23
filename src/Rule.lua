local Chunk = require 'Chunk'
local Log   = require 'Log'

local Rule = Chunk:new()

function Rule:new(def)
    def = def or {}
    setmetatable(def, self)
    self.__index = self
    return def
end

function Rule:match(value, pattern, state)
    if type(pattern) == 'function' then
        return pattern(value, state)
    elseif type(value) ~= type(pattern) then
        return false
    elseif type(value) == 'table' then
        for k, v in pairs(pattern) do
            if not self:match(value[k], v, state) then
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

function Rule:merge(to, from, state)
    for key, value in pairs(from or {}) do
        if type(key) ~= 'number' then
            if type(key) == 'function' then
                key = key(nil, state)
            end
            if type(value) == 'function' then
                value = value(nil, state)
            end
            if type(value) == 'table' then
                if type(to[key]) ~= 'table'  then
                    to[key] = {}
                end
                to[key] = self:merge(to[key], value, state)
            else
                to[key] = value
            end
        end
    end
    for i, val in ipairs(from or {}) do
        if type(val) == 'function' then
            val = val(nil, state)
        end
        if val == nil then
            to[i] = nil
        elseif type(val) == 'table' then
            local v = self:merge({}, val, state)
            if next(v) then
                table.insert(to, v)
            end
        else
            table.insert(to, val)
        end
    end
    return to
end

return Rule
