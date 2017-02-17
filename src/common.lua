-- common

local function assert_arg(fname, num, expected, value)
    local got = type(value)
    assert(got == expected,
        string.format("bad argument #%d to '%s' (%s expected, got %s)",
            num, fname, expected, got))
end

function apply_all(...)
    local procs = {...}
    return function (...)
        for _, proc in ipairs(procs) do
            proc(...)
        end
    end
end

function compose(f, g)
    return function (...)
        return f(g(...))
    end
end

function append(ls, ...)
    assert_arg('append', 1, 'table', ls)
    for _, arg in ipairs({...}) do
        table.insert(ls, arg)
    end
    return ls
end

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

function each(t)
    local i, n = 0, #t
    return function ()
        i = i + 1
        if i <= n then return t[i] end
    end
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

function string.escape(s)
    assert_arg('escape', 1, 'string', s)
    return string.gsub(s, '([^%w_-])', '\\%1')
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

function string.split2(s, sep)
    local o = {}
    for val in string.gmatch(s, '[^'..sep..']+') do
        table.insert(o, val)
    end
    return o
end

-- compatibility with Lua 5.1
if type(table.unpack) ~= 'function' then
    function table.unpack(t)
        return unpack(t)
    end
end

function table.rest(t, start)
    local o = {}
    for i = start, #t do
        table.insert(o, t[i])
    end
    return o
end

function table.iextend(this, other)
    assert_arg('iextend', 1, 'table', this)
    assert_arg('iextend', 2, 'table', other)
    for _, i in ipairs(other) do
        table.insert(this, i)
    end
    return this
end

function table.map(t, func)
    assert_arg('map', 1, 'table', t)
    assert_arg('map', 2, 'function', func)
    local out = {}
    for k, v in pairs(t) do
        out[k] = func(v)
    end
    return out
end

function table.imap(t, func)
    assert_arg('imap', 1, 'table', t)
    assert_arg('imap', 2, 'function', func)
    local out = {}
    for _, v in ipairs(t) do
        table.insert(out, func(v))
    end
    return out
end

function table.for_each(t, func)
    assert_arg('for_each', 1, 'table', t)
    assert_arg('for_each', 2, 'function', func)
    for _, v in pairs(t) do
        func(v)
    end
end

function table.filter(t, pred)
    assert_arg('filter', 1, 'table', t)
    assert_arg('filter', 2, 'function', pred)
    local out = {}
    for k, v in pairs(t) do
        if pred(v) then
            out[k] = v
        end
    end
    return out
end

function table.find(t, pred)
    assert_arg('find', 1, 'table', t)
    assert_arg('find', 2, 'function', pred)
    for k, v in pairs(t) do
        if pred(v) then
            return v, i
        end
    end
    return nil, nil
end

function table.merge(to, from)
    assert_arg('merge', 1, 'table', to)
    assert_arg('merge', 2, 'table', from)
    for k, v in pairs(from) do
        if type(k) ~= 'number' then
            if type(v) == 'table' then
                to[k] = table.merge(to[k] or {}, v)
            else
                to[k] = v
            end
        end
    end
    for _, v in ipairs(from) do
        table.insert(to, v)
    end
    return to
end

function table.imove(to, from)
    assert_arg('imove', 1, 'table', to)
    assert_arg('imove', 2, 'table', from)
    for i, v in ipairs(from) do
        table.insert(to, v)
        from[i] = nil
    end
    return to
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

function io.read_line(file)
    return file:read('*l')
end

function io.read_single_line(file)
    local first, second = file:read('*l', '*l')
    assert(not second, 'unexpected second line read')
    return first
end
