local Chunk = {}

function Chunk:new(obj)
    local obj = obj or {}
    self._string_ = tostring(obj)
    setmetatable(obj, self)
    self.__index = self
    return obj
end

function Chunk:each()
    return function (_, prev)
        local key, value = prev, nil
        repeat
            key, value = next(self, key)
        until key == nil
              or  type(key) == 'string'
              and key:sub(1, 1) ~= '_'
        return key, value
    end
end

function Chunk:__tostring()
    return self._name_ or self._string_
end

return Chunk
