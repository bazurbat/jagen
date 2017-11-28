-- common

local insert = table.insert
local concat = table.concat
local format = string.format

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
    if not list then return end
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

function quote(arg)
    assert_arg('quote', 1, 'string', arg)
    return string.format('"%s"', arg)
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

function string.join(t, sep)
    local o = {}
    for _, s in ipairs(t) do
        if #s > 0 then
            table.insert(o, s)
        end
    end
    return table.concat(o, sep or '')
end

function string.trim(s)
    return string.match(s, '^%s*(.-)%s*$')
end

function string.remove_prefix(str, prefix)
    local b, e = string.find(str, prefix, 1, true)
    if e then
        return string.sub(str, e+1), true
    else
        return str, false
    end
end

function string.escape_pattern(p)
    return string.gsub(p, '%p', '%%%0')
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

function table.rest(list, start)
    assert_arg('rest', 1, 'table', list)
    local out = {}
    for i = start or 1, #list do
        table.insert(out, list[i])
    end
    return out
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

function table.ifilter(list, pred)
    assert_arg('ifilter', 1, 'table', list)
    assert_arg('ifilter', 2, 'function', pred)
    local matching, rest = {}, {}
    for i, v in ipairs(list) do
        if pred(v, i) then
            table.insert(matching, v)
        else
            table.insert(rest, v)
        end
    end
    return matching, rest
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

function table.count(this)
    local count = 0
    if this then
        for k, v in pairs(this) do
            count = count + 1
        end
    end
    return count
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

function pretty(value, level)
    local level = level or 0
    local output = {}

    local function add(line, indent)
        insert(output, format('%s%s', string.rep('  ', indent or 0), line))
    end
    local function isarray(t)
        if type(t) ~= 'table' then
            return false
        end
        for k, v in pairs(t) do
            if type(k) ~= 'number' then
                return false
            end
        end
        return true
    end
    local function pretty_array(t)
        local strings = {}
        for k, v in ipairs(t) do
            insert(strings, pretty(v))
        end
        return concat(strings, ', ')
    end

    if type(value) == 'table' then
        if isarray(value) then
            return format('[ %s ]', pretty_array(value))
        else
            local keys = {}
            for k, _ in pairs(value) do
                if type(k) ~= 'number' then insert(keys, k) end
            end
            table.sort(keys)
            add('{')
            for _, k in ipairs(keys) do
                add(format('%s = %s', k, pretty(value[k], level+1)), level+1)
            end
            for k, v in ipairs(value) do
                add(pretty(v, level+1), level+1)
            end
            add('}', level)
        end
    elseif type(value) == 'string' then
        add(format("'%s'", value))
    else
        add(format("%s", value))
    end
    return concat(output, '\n')
end
