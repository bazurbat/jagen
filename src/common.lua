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

function append(list, ...)
    list = list or {}
    for _, arg in ipairs({...}) do
        table.insert(list, arg)
    end
    return list
end

function extend(list, other_list)
    list = list or {}
    for _, val in ipairs(other_list or {}) do
        table.insert(list, val)
    end
    return list
end

function sort(list, comp)
    table.sort(list, comp)
    return list
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

function map(func, list)
    local new_list = {}
    for _, val in ipairs(list or {}) do
        table.insert(new_list, func(val))
    end
    return new_list
end

function pmap(func, list)
    local new_list = {}
    for key, val in pairs(list) do
        table.insert(new_list, func(key, val))
    end
    return new_list
end

function for_each(list, func, ...)
    for _, v in ipairs(list) do
        func(v, ...)
    end
end

function find(pred, list)
    for i, v in ipairs(list) do
        if pred(v) then
            return v, i
        end
    end
    return nil, nil
end

function contains(member, list)
    for i, v in ipairs(list) do
        if v == member then
            return true, i
        end
    end
    return false, nil
end

function append_uniq(value, list)
    if not contains(value, list) then
        table.insert(list, value)
    end
    return list
end

function string.empty(s)
    return not s or #s == 0
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

function table.keys(t)
    local keys = {}
    for k, v in pairs(t or {}) do
        table.insert(keys, k)
    end
    return keys
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

function toppstring(value, indent)
    local indent = indent or 0
    local output = {}

    local function add(formatstring, ...)
        table.insert(output, string.format(formatstring, ...))
    end
    local function isarray(t)
        if type(t) ~= 'table' then
            return false
        end
        for k, v in pairs(t) do
            if type(k) ~= 'number' or type(v) == 'table' then
                return false
            end
        end
        return true
    end
    local function fmt_indent(indent)
        return string.rep(' ', indent)
    end
    local function fmt_array(t)
        local strings = {}
        for k, v in ipairs(t) do
            table.insert(strings, toppstring(v))
        end
        return table.concat(strings, ', ')
    end
    local function fmt_kv(k, v)
        return string.format('%s%s = %s',
            fmt_indent(indent+2), k, toppstring(v, indent+2))
    end

    if type(value) == 'table' then
        if isarray(value) then
            return string.format('[ %s ]', fmt_array(value))
        else
            table.insert(output, '{')
            for k, v in pairs(value) do
                if type(k) ~= 'number' then
                    add('%s', fmt_kv(k, v))
                end
            end
            if #value > 0 then
                local a = {}
                for k, v in ipairs(value) do
                    if type(v) == 'table' then
                        add('%s', fmt_kv(k, v))
                    else
                        table.insert(a, tostring(v))
                    end
                end
                if #a > 0 then
                    add("%s%s", fmt_indent(indent+2), fmt_array(a))
                end
            end
        end
        add('%s}', fmt_indent(indent))
    elseif type(value) == 'string' then
        add("'%s'", value)
    else
        add("%s", tostring(value))
    end
    return table.concat(output, '\n')
end
