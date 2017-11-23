local Log = require 'Log'

local P = {}
P.__index = P

function P:new(config)
    local o = {
        config = {}
    }
    setmetatable(o, self)

    for _, item in ipairs(config or {}) do
        local opt, name, long, short = {}
        if type(item[1]) == 'string' then
            name = item[1]
            table.remove(item, 1)
        end
        if type(item[1]) == 'function' then
            opt.func = item[1]
            table.remove(item, 1)
        end
        if string.sub(name, -1) == '=' then
            opt.needs_value = true
            name = string.sub(name, 1, -2)
        end
        long, short = string.match(name, '(%w+),(%w)')
        if not long then long = name end
        opt.long = long
        opt.short = short
        o.config[opt.long] = opt
    end

    return o
end

function P:is_option(arg)
    return string.sub(arg, 1, 1) == '-'
end

function P:is_short(arg)
    return string.sub(arg, 1, 1) == '-' and not string.sub(arg, 2, 1) == '-'
end

function P:match(opt, arg)
    local patterns = {
        string.format('^--(%s)$', opt.long),
        string.format('^--(%s)=(.*)$', opt.long)
    }
    if opt.short then
        table.insert(patterns, string.format('^-(%s)$', opt.short))
        table.insert(patterns, string.format('^-(%s)(.+)$', opt.short))
    end
    local name, value
    for _, pattern in ipairs(patterns) do
        name, value = string.match(arg, pattern)
        if name then
            return name, value
        end
    end
end

function P.parse(args, config)
    local options = P:new(config)
    local next_opt
    local function match(arg)
        for _, opt in pairs(options.config) do
            local name, value = options:match(opt, arg)
            if name then
                if opt.needs_value and not value then
                    next_opt = opt
                    return true
                else
                    opt.func(value)
                    return true
                end
            end
        end
    end
    for i = 1, #args do
        local arg = args[i]
        if next_opt then
            if not options:is_option(arg) then
                next_opt.func(arg)
            end
            next_opt = nil
        else
            if not match(arg) then
                Log.error('unexpected argument: %s', arg)
                return
            end
        end
    end
    if next_opt then
        next_opt.func()
    end
    return true
end

return P
