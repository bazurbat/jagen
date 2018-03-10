local Log = require 'Log'

local Command = {}

function Command:new(...)
    local o = { command = {...} }
    setmetatable(o, self)
    self.__index = self
    return o
end

function Command:append(...)
    append(self.command, ...)
    return self
end

function Command:__tostring()
    return table.concat(self.command, ' ')
end

function Command:exists()
    return Command:new('command -v', tostring(self)):read()
end

function Command:exec()
    local cmdstr = tostring(self) Log.debug2(cmdstr)
    local ok = os.execute(cmdstr)
    if type(ok) == 'number' then
        return ok == 0 -- Lua 5.1
    else
        return ok      -- Lua 5.2
    end
end

function Command:popen(mode)
    local command = tostring(self) Log.debug2(command)
    return assert(io.popen(command, mode))
end

function Command:read(...)
    local file = self:popen()
    local results = { file:read(...) }
    file:close()
    return table.unpack(results)
end

return Command
