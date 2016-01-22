function copy(o)
    if type(o) == 'table' then
        local c = {}
        for k, v in pairs(o) do
            c[k] = copy(v)
        end
        return c
    else
        return o
    end
end

function each(a)
    local i, n = 0, #a
    return function (t)
        i = i + 1
        if i <= n then return t[i] end
    end, a
end

function append(...)
    local o = {}
    for _, arg in ipairs({...}) do
        for _, i in ipairs(arg) do
            table.insert(o, i)
        end
    end
    return o
end

function map(f, t)
    local r = {}
    for i, v in ipairs(t or {}) do
        table.insert(r, f(v))
    end
    return r
end

function find(pred, list)
    for i, v in ipairs(list) do
        if pred(v) then
            return v, i
        end
    end
    return nil, nil
end

function string.split(s, sep)
    local o, b, e = {}
    local init = 1

    repeat
        b, e = string.find(s, sep, init, true)
        if not b then b = 0 end
        table.insert(o, string.sub(s, init, b-1))
        if e then init = e + 1 end
    until b == 0

    return o
end

function table.rest(t, start)
    local o = {}
    for i = start, #t do
        table.insert(o, t[i])
    end
    return o
end

function table.merge(a, b)
    for k, v in pairs(b) do
        if type(k) ~= 'number' then
            if type(v) == 'table' then
                a[k] = table.merge(a[k] or {}, v)
            else
                a[k] = v
            end
        end
    end
    for _, v in ipairs(b) do
        table.insert(a, v)
    end
    return a
end

function table.dump(t, i)
    local i = i or 0
    if type(t) ~= 'table' then
        io.write(tostring(t), '\n')
        return
    end
    io.write(string.rep(' ', i), tostring(t), ' {\n')
    for k, v in pairs(t) do
        io.write(string.rep(' ', i+2), k, ' = ')
        if type(v) == 'table' then
            io.write('\n')
            table.dump(v, i+4)
        else
            io.write(tostring(v), '\n')
        end
    end
    io.write(string.rep(' ', i), '}\n')
end

system = {}

function system.mkpath(...)
    local sep = '/'
    local path = {}
    for _, c in ipairs({...}) do
        table.insert(path, c)
    end
    return table.concat(path, sep)
end

function system.tocommand(...)
    local command = {}
    for _, arg in ipairs({...}) do
        table.insert(command, string.format('%s', tostring(arg)))
    end
    return table.concat(command, ' ')
end

function system.exec(...)
    local command = system.tocommand(...)
    jagen.debug1(command)
    local status = os.execute(command)
    return status == 0, status % 0xFF
end

function system.popen(...)
    local command = system.tocommand(...)
    jagen.debug1(command)
    return io.popen(command)
end

function system.exists(pathname)
    assert(type(pathname) == 'string')
    return os.execute(string.format('test -e "%s"', pathname)) == 0
end

function import_paths(filename)
    local o = {}
    table.insert(o, system.mkpath(jagen.dir, 'lib', filename))
    for _, overlay in ipairs(string.split(jagen.overlays, ' ')) do
        table.insert(o, system.mkpath(jagen.dir, 'overlay', overlay, filename))
    end
    table.insert(o, system.mkpath(jagen.root, filename))
    return o
end
