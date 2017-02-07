local P = {}

local Log = require 'Log'

function P.quote(...)
    local function quote(arg)
        return string.format('%q', tostring(arg))
    end
    return table.concat(table.imap({...}, quote), ' ')
end

function P.expand(...)
    return P.pread('*a', 'printf "%s" '..P.quote(...), '%s')
end

function P.mkpath(...)
    local sep = '/'
    local path = {}
    for _, c in ipairs({...}) do
        table.insert(path, c)
    end
    return table.concat(path, sep)
end

function P.exec(cmdline, ...)
    Log.debug2(cmdline, ...)
    local command = string.format(cmdline, ...)
    local status = os.execute(command)
    -- handling API change in Lua 5.2
    if type(status) == 'number' then
        return status == 0, status % 0xFF
    else
        return status or false
    end
end

function P.popen(cmdline, ...)
    Log.debug2(cmdline, ...)
    local prog = string.format(cmdline, ...)
    return assert(io.popen(prog))
end

function P.pipe(func, command, reader)
    Log.debug2(command)
    local file = assert(io.popen(command))
    local vals
    if reader then
        vals = { func(reader(file)) }
    else
        vals = { func(file) }
    end
    file:close()
    return table.unpack(vals)
end

function P.pread(format, cmdline, ...)
    local file = P.popen(cmdline, ...)
    local out = file:read(format)
    file:close()
    return out
end

function P.with_output_file(filename, proc)
    local file = assert(io.open(filename, 'wb'))
    local values = { proc(file) }
    file:close()
    return unpack(values)
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
    return P.exec('rm -rf %s', P.quote(...))
end

function P.mkdir(...)
    return P.exec('mkdir -p %s', P.quote(...))
end

function P.exists(path)
    return P.exec('test -e "%s"', path)
end

function P.file_exists(path)
    return P.exec('test -f "%s"', path)
end

function P.dir_exists(path)
    return P.exec('test -d "%s"', path)
end

function P.is_empty(path)
    return P.pread('*l', 'cd "%s" && echo *', path) == '*'
end

return P
