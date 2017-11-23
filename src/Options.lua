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
            opt.need_value = true
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

function P:match(opt, arg)
    return string.match(arg, '%-%-'..opt.long) or
        opt.short and string.match(arg, '%-'..opt.short)
end

function P.parse(args, config)
    local options = P:new(config)
    local next_opt
    for i = 1, #args do
        local arg = args[i]
        if next_opt then
            if not options:is_option(arg) then
                next_opt.func(arg)
            end
            next_opt = nil
        else
            for _, opt in pairs(options.config) do
                if options:match(opt, arg) then
                    local _, eq, value
                    if opt.need_value then
                        _, eq, value = string.match(arg, '([^=]+)(=)(.+)')
                        if eq then
                            opt.func(value)
                        else
                            next_opt = opt
                        end
                    else
                        opt.func(value)
                    end
                end
            end
        end
    end
    if next_opt then
        next_opt.func()
    end
end

return P
