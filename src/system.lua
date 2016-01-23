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

return P
