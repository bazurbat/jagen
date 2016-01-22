local P = {}

function P.mkpath(...)
    local sep = '/'
    local path = {}
    for _, c in ipairs({...}) do
        table.insert(path, c)
    end
    return table.concat(path, sep)
end

function P.tocommand(...)
    local command = {}
    for _, arg in ipairs({...}) do
        table.insert(command, string.format('%s', tostring(arg)))
    end
    return table.concat(command, ' ')
end

function P.exec(...)
    local command = P.tocommand(...)
    jagen.debug1(command)
    local status = os.execute(command)
    return status == 0, status % 0xFF
end

function P.popen(...)
    local command = P.tocommand(...)
    jagen.debug1(command)
    return io.popen(command)
end

function P.exists(pathname)
    assert(type(pathname) == 'string')
    return os.execute(string.format('test -e "%s"', pathname)) == 0
end

function P.import_paths(filename)
    local o = {}
    table.insert(o, P.mkpath(jagen.dir, 'lib', filename))
    for _, overlay in ipairs(string.split(jagen.overlays, ' ')) do
        table.insert(o, P.mkpath(jagen.dir, 'overlay', overlay, filename))
    end
    table.insert(o, P.mkpath(jagen.root, filename))
    return o
end

return P
