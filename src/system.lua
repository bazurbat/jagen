local P = {}

function P.mkpath(...)
    local sep = '/'
    local path = {}
    for _, c in ipairs({...}) do
        table.insert(path, c)
    end
    return table.concat(path, sep)
end

local function tocommand(...)
    local command = {}
    for _, arg in ipairs({...}) do
        table.insert(command, string.format('%s', tostring(arg)))
    end
    return table.concat(command, ' ')
end

function P.exec(...)
    local command = tocommand(...)
    jagen.debug1(command)
    local status = os.execute(command)
    return status == 0, status % 0xFF
end

function P.popen(...)
    local command = tocommand(...)
    jagen.debug1(command)
    return io.popen(command)
end

function P.getenv(vars)
    local o = {}
    for _, v in ipairs(vars) do
        local value = os.getenv(v)
        assert(value, string.format('the environment variable is not set: %s', v))
        table.insert(o, value)
    end
    return o
end

function P.rmrf(...)
    return P.exec('rm', '-rf', ...)
end

function P.mkdir(...)
    return P.exec('mkdir', '-p', ...)
end

return P
