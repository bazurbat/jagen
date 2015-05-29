
function mkpath(...)
    local sep = '/'
    local path = {}
    for _, c in ipairs({...}) do
        table.insert(path, c)
    end
    return table.concat(path, sep)
end

function mkdir(pathname)
    exec('mkdir -p "' .. pathname .. '"')
end

function file_newer(file1, file2)
    local cmd = string.format('[ "%s" -nt "%s" ]', file1, file2)
    return os.execute(cmd) == 0
end

function file_older(file1, file2)
    local cmd = string.format('[ "%s" -ot "%s" ]', file1, file2)
    return os.execute(cmd) == 0
end

local function exec(command, ...)
    local cmd = { command }
    for _, arg in ipairs({...}) do
        table.insert(cmd, string.format('%q', arg))
    end
    local status = os.execute(table.concat(cmd, ' '))
    return status
end

function env(name)
    return os.getenv(name)
end
