local Log = require 'Log'

local Command = {}

function Command:new(...)
    local o = { command = {...} }
    setmetatable(o, self)
    self.__index = self
    return o
end

function Command:newf(f, ...)
    return Command:new(string.format(f, ...))
end

function Command:append(...)
    append(self.command, ...)
    return self
end

function Command:__tostring()
    return table.concat(self.command, ' ')
end

function Command:exists()
    return Command:new('command -v', self.command[1]):read()
end

function Command:exec()
    local cmdstr = tostring(self) Log.debug1(cmdstr:escape_format())
    local ok = os.execute(cmdstr)
    if type(ok) == 'number' then
        return ok == 0 -- Lua 5.1
    else
        return ok      -- Lua 5.2
    end
end

function Command:popen(mode)
    local command = tostring(self) Log.debug1(command:escape_format())
    return assert(io.popen(command, mode))
end

function Command:lines()
    local pipe = self:popen()
    local lines = pipe:lines()
    return function ()
        local line = lines()
        if line ~= nil then
            return line
        else
            pipe:close()
        end
    end
end

function Command:match(pattern)
    for line in self:lines() do
        local m = line:match(pattern)
        if m then return m end
    end
end

function Command:aslist()
    local pipe = self:popen()
    local list = aslist(pipe:lines())
    pipe:close()
    return list
end

function Command:read(...)
    local file = self:popen()
    local results = { file:read(...) }
    file:close()
    return table.unpack(results)
end

return Command
