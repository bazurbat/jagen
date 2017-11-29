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
    return arg and string.sub(arg, 1, 1) == '-'
end

function P:is_short(arg)
    return arg and string.sub(arg, 1, 1) == '-' and string.sub(arg, 2, 2) ~= '-'
end

function P:is_long(arg)
    return arg and string.sub(arg, 1, 1) == '-' and string.sub(arg, 2, 2) == '-'
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
    local short_opts = table.filter(options.config, function (opt) return opt.short end)
    local long_opts  = table.filter(options.config, function (opt) return opt.long end)
    local i = 1
    while i <= #args do
        local arg = args[i]
        if options:is_short(arg) then
            local j = 2
            while j <= #arg do
                local arg_name, name, value, opt = string.sub(arg, j)
                for _, o in pairs(short_opts) do
                    local patterns = {
                        string.format('^(%s)$', o.short),
                        string.format('^(%s)=(%%S*)$', o.short),
                        string.format('^(%s)(%%S*)$', o.short),
                    }
                    for _, pattern in ipairs(patterns) do
                        name, value = string.match(arg_name, pattern)
                        if name then break end
                    end
                    if name then opt = o break end
                end
                if not opt then
                    Log.error("invalid option: -%s", string.sub(arg_name, 1, 1))
                    return false
                end
                if opt.needs_value then
                    local next_arg = args[i+1]
                    if not value and not options:is_option(next_arg) then
                        value = next_arg
                        i = i+1
                    end
                    j = #arg+1
                end
                opt.func(value)
                j = j+1
            end
        elseif options:is_long(arg) then
            local arg_name, name, value, opt = string.sub(arg, 3)
            for _, o in pairs(long_opts) do
                local patterns = {
                    string.format('^(%s)$', o.long),
                    string.format('^(%s)=(%%S*)$', o.long)
                }
                for _, pattern in ipairs(patterns) do
                    name, value = string.match(arg_name, pattern)
                    if name then break end
                end
                if name then opt = o break end
            end
            if not opt then
                Log.error("invalid option: %s", arg)
                return false
            end
            if opt.needs_value then
                local next_arg = args[i+1]
                if not value and not options:is_option(next_arg) then
                    value = next_arg
                    i = i+1
                end
            end
            opt.func(value)
        end
        i = i+1
    end
    return true
end

return P
