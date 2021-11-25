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
        -- return Rule.match(value, pattern(state, value), state)
        -- print(value, pattern(state, value), Rule.match(value, pattern(state, value), state))
        return pattern(state, value)
    elseif type(value) ~= type(pattern) then
        return false, 1
    elseif type(value) == 'table' then
        for k, v in pairs(pattern) do
            if not Rule.match(value[k], v, state) then
                return false, 2
            end
        end
    elseif type(value) == 'string'
           and not string.match(value, pattern) then
        return false, 3
    elseif type(value) ~= 'string' and value ~= pattern then
        return false, 4
    end
    return true, state
end

function Rule.merge(to, from, state, debug)
    if debug then
        Log.debug2('merging %s', pretty(from))
    end
    for key, value in pairs(from or {}) do
        local tkey, tvalue = type(key), type(value)
        if debug then
            Log.debug2('%s: %s', tostring(key), tostring(value))
        end
        if tkey ~= 'number' then
            if tkey == 'function' then
                key = key(state)
            end
            if tvalue == 'function' then
                value = value(state, to[key])
            end
            if tvalue == 'table' then
                if type(to[key]) ~= 'table'  then
                    to[key] = {}
                end
                to[key] = Rule.merge(to[key], value, state, debug)
            else
                to[key] = value
            end
        end
    end

    for i, value in ipairs(from or {}) do
        if debug then
            Log.debug2('%d: %s %s', i, value, pretty(state.value))
        end
        if type(value) == 'function' then
            if debug then
                Log.debug2('%d: %s %s (expanded)', i, value, value(state) or 'nil')
            end
            value = value(state)
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
