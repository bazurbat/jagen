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
    jagen.debug2(command)
    local status = os.execute(command)
    return status == 0, status % 0xFF
end

function P.popen(...)
    local command = tocommand(...)
    jagen.debug1(command)
    return io.popen(command)
end

function P.pipe(func, ...)
    local command = tocommand(...)
    jagen.debug1(command)
    local file = assert(io.popen(command))
    local o = { func(file) }
    file:close()
    return unpack(o)
end

function P.getenv(vars)
    local o = {}
    for _, v in ipairs(vars) do
        local value = os.getenv(v)
        assert(value and #value > 0,
            string.format("the environment variable '%s' is not set", v))
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

function P.exists(path)
    return P.exec(string.format('test -e "%s"', path))
end

function P.file_exists(pathname)
    return P.exec(string.format('test -f "%s"', pathname))
end

function P.is_empty(path)
    local pipe = assert(P.popen(string.format('cd "%s" && echo *', path)))
    local empty = assert(pipe:read()) == '*'
    pipe:close()
    return empty
end

return P
