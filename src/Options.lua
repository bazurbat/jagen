local Log = require 'Log'

local P = {}
P.__index = P

function P:new(config)
    local this = {
        config = { long = {}, short = {} },
        init = {}
    }
    setmetatable(this, self)

    for _, item in ipairs(config or {}) do
        local opt, cfg = {}, ''
        if type(item[1]) == 'string' then
            cfg = item[1]
            table.remove(item, 1)
        end
        local names, value = table.unpack(cfg:split('='))
        local long, short = table.unpack(names:split(','))
        opt.long = assert(long)
        if short then opt.short = short end
        if value then
            opt.needs_value = value
        end
        if item[1] ~= nil then
            this.init[long] = item[1]
            table.remove(item, 1)
        else
            this.init[long] = false
        end
        if item[1] ~= nil then
            opt.max_value = item[1]
            table.remove(item, 1)
        end
        this.config.long[long] = opt
        if short and #short > 0 then
            this.config.short[short] = opt
        end
    end

    return this
end

function P:is_option(arg)
    return string.sub(arg, 1, 1) == '-'
end

function P:is_short(arg)
    return arg and string.sub(arg, 1, 1) == '-' and string.sub(arg, 2, 2) ~= '-'
end

function P:is_long(arg)
    return arg and string.sub(arg, 1, 1) == '-' and string.sub(arg, 2, 2) == '-'
end

function P:is_eoa(arg)
    return P:is_long(arg) and #arg == 2
end

local function result_tostring(self)
    local lines = {}
    local function append(value)
        table.insert(lines, value)
    end
    for k, v in pairs(self) do
        if v == true then
            append(string.format('--%s', k))
        end
    end
    return table.concat(lines, ' ')

end

function P:parse(args)
    local result = copy(self.init)
    setmetatable(result, { __tostring = result_tostring })
    local read_nth, eoa

    local function set_value(n, opt, val)
        if opt.needs_value == 'n' then
            local num = tonumber(val)
            if not num then
                Log.error("option '%s' (%s) requires a number value but the specified value '%s' is not a number",
                    opt.long, opt.short, val)
                return
            end
            result[opt.long] = num
        else
            result[opt.long] = val
        end
        return true
    end

    local function read_value(n, opt)
        local arg = args[n]
        local value
        if arg and not self:is_option(arg) then
            value = arg
        else
            value = opt.max_value
        end
        if not set_value(n, opt, value) then
            return
        end
        return read_nth(n+1)
    end

    local function read_long(n)
        local patterns = { '^%-%-([%w-]+)=(.*)$', '^%-%-([%w-]+)' }
        local arg = args[n]
        local name, value
        for pattern in each(patterns) do
            name, value = string.match(arg, pattern)
            if name then break end
        end
        local opt = self.config.long[name]
        if not opt then
            Log.error('invalid option: %s', arg)
            return
        end
        if opt.needs_value then
            if value then
                if not set_value(n, opt, value) then return end
            else
                return read_value(n+1, opt)
            end
        else
            result[name] = true
        end
        return read_nth(n+1)
    end

    local function read_optstring(n, optstring)
        local a = string.sub(optstring, 1, 1)
        if #a < 1 then
            return read_nth(n+1)
        end
        local rest = string.sub(optstring, 2)
        local opt = self.config.short[a]
        if not opt then
            Log.error('invalid option: -%s', a)
            return
        end
        if opt.needs_value then
            if #rest > 0 then
                if set_value(n, opt, rest) then
                    return read_nth(n+1)
                else
                    return
                end
            else
                return read_value(n+1, opt)
            end
        else
            result[opt.long] = true
            return read_optstring(n, rest)
        end
    end

    local function read_short(n)
        local arg = args[n]
        return read_optstring(n, string.sub(arg, 2))
    end

    read_nth = function (n)
        local arg = args[n]
        if not arg then return result end
        if eoa then
            result._args = append(result._args, args[n])
        else
            if self:is_eoa(arg) then
                eoa = true
            elseif self:is_long(arg) then
                return read_long(n)
            elseif self:is_short(arg) then
                return read_short(n)
            else
                table.insert(result, args[n])
            end
        end
        return read_nth(n+1, args)
    end

    return read_nth(1)
end

return P
