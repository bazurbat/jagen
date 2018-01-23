local Log = require 'Log'

local insert = table.insert
local format = string.format

local function quote_arg(arg)
    if string.match(arg, '[^%w_/.+-]') then
        return format('"%s"', arg)
    else
        return arg
    end
end

local function make_args(argv)
    if type(argv) == 'table' then
        local args = {}
        for key, val in pairs(argv) do
            if type(key) == 'string' then
                local dash, op = '--', '='
                if #key == 1 then dash = '-' op = ' ' end
                if val == true then
                    insert(args, format('%s%s', dash, key))
                elseif val == false then
                else
                    insert(args, format('%s%s%s%s', dash, key, op, quote_arg(val)))
                end
            elseif type(key) == 'number' then
            end
        end
        for _, val in ipairs(argv) do
            table.insert(args, make_args(val))
        end
        return table.concat(args, ' ')
    else
        return argv
    end
end

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

function Command:exec()
    local command = tostring(self) Log.debug2(command)
    local status, kind, num = os.execute(command)
    if type(status) == 'number' then        -- Lua 5.1
        return status == 0, status % 0x7F
    else                                    -- Lua 5.2
        return status or false, num % 0x7F
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
