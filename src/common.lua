-- common

local insert = table.insert
local concat = table.concat
local format = string.format

-- Lua 5.1/5.2 compatibility
if not table.unpack and type(unpack) == 'function' then
    table.unpack = unpack
elseif not unpack and type(table.unpack) == 'function' then
    unpack = table.unpack
end

local function assert_arg(fname, num, expected, value)
    local got = type(value)
    assert(got == expected,
        string.format("bad argument #%d to '%s' (%s expected, got %s)",
            num, fname, expected, got))
end

function comp(...)
    local fs = {...}
    return function(...)
        local rs = {...}
        for i = 1, #fs do
            rs = { fs[i](unpack(rs)) }
        end
        return unpack(rs)
    end
end

function inext(t, i)
    if not i then i = 0 end
    i = i+1
    local v = t[i]
    if v then
        return i, v
    end
end

function iter(t, itt)
    if itt then
        return itt(next), t
    else
        return next, t
    end
end

function iiter(t, itt)
    if itt then
        return itt(inext), t, 0
    else
        return inext, t
    end
end

function tolist(t, itt)
    local o = {}
    for i, v in itt(inext), t, 0 do
        insert(o, v)
    end
    return o
end

function map(f, t)
    if t then
        return tolist(t, map(f))
    else
        return function(iter)
            return function(t, last)
                local k, v = iter(t, last)
                if k then return k, f(v, k) end
            end
        end
    end
end

function filter(pred, t)
    if t then
        return tolist(t, filter(pred))
    else
        return function(iter)
            local function step(t, last)
                local k, v = iter(t, last)
                if k then
                    if pred(v, k) then
                        return k, v
                    else
                        return step(t, k)
                    end
                end
            end
            return step
        end
    end
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
    for i = 1, select('#', ...) do
        local val = select(i, ...)
        if val ~= nil then
            insert(list, val)
        end
    end
    return list
end

function prepend(list, ...)
    return extend({...}, list)
end

function extend(list, other)
    list, other = list or {}, other or {}
    for i = 1, #other do
        local val = other[i]
        if val ~= nil then
            insert(list, val)
        end
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
        setmetatable(c, getmetatable(o))
        for k, v in pairs(o) do
            c[k] = copy(v)
        end
        return c
    else
        return o
    end
end

function each(...)
    local i, j, ts = 1, 1, {...}
    local function iter()
        local t = ts[i]
        if t == nil then return end
        local v = t[j]
        if v == nil then
            i, j = i + 1, 1
            return iter()
        else
            j = j + 1
        end
        return v
    end
    return iter
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
    if list then
        for i = 1, #list do
            v = list[i]
            if v == member then
                return true, i
            end
        end
    end
    return false, nil
end

function append_uniq(value, list)
    list = list or {}
    if not contains(value, list) then
        insert(list, value)
    end
    return list
end

function quote(...)
    local n = select('#', ...)
    if n == 1 then
        return format('"%s"', tostring((select(1, ...))))
    else
        local result = {}
        for i = 1, n do
            insert(result, format('"%s"', tostring((select(i, ...)))))
        end
        return unpack(result)
    end
end

function squote(...)
    local result = {}
    for arg in each {...} do
        table.insert(result, string.format("'%s'", tostring(arg)))
    end
    return unpack(result)
end

function string.empty(s)
    return not s or #s == 0
end

function string.escape(s)
    assert_arg('escape', 1, 'string', s)
    return string.gsub(s, '([^%w_-])', '\\%1')
end

-- this one preserves empty fields:
--     split('=a==b=', '=') -> [ '', 'a', '', 'b', '' ]
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

-- does not preserve empty fields:
--     split2('=a==b=', '=') -> [ 'a', 'b' ]
function string.split2(s, sep)
    sep = sep or ' '
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

function string.to_identifier(s)
    return string.gsub(s, '[^%w_]', '_')
end

function string.escape_pattern(p)
    return string.gsub(p, '%p', '%%%0')
end

function string.convert_pattern(s)
    s = string.escape_pattern(s)
    s = string.format('^%s$', s)
    s = string.gsub(s, '%%%?', '[%%w%%p]')
    s = string.gsub(s, '%%%*', '[%%w%%p]*')
    return s
end

function string.to_target_pattern(s)
    if #s == 0 then return s end
    if not (s:match(':') or s:match('*') or s:match('?')) then
        s = s..':*'
    end
    s = s:gsub('::', ':*:')
    s = s:gsub('^:', '*:')
    s = s:gsub(':$', ':*')
    s = s:escape_pattern(s)
    s = string.format('^%s$', s)
    s = s:gsub('%%%?', '[%%w%%p]')
    s = s:gsub('%%%*', '[%%w%%p]*')
    return s
end

function string.tocanon(s)
    s = string.lower(s)
    s = string.gsub(s, '_', '-')
    return s
end

function table.tolist(t)
    local list = {}
    for k, v in pairs(t) do
        table.insert(list, v)
    end
    return list
end

function table.iclean(t)
    if not t then return end
    for i, _ in ipairs(t) do
        t[i] = nil
    end
end

-- Returns a shallow copy of the supplied table.
function table.copy(t)
    assert_arg('copy', 1, 'table', t)
    local o = {}
    for k, v in next, t do
        o[k] = v
    end
    return o
end

function table.keys(t)
    local keys = {}
    for k, v in pairs(t or {}) do
        if type(k) ~= 'number' then
            table.insert(keys, k)
        end
    end
    return keys
end

function table.ivalues(t)
    local values = {}
    for _, v in ipairs(t or {}) do
        insert(values, v)
    end
    return values
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
    local insert, concat, format = table.insert, table.concat, string.format

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
            insert(strings, pretty(v, level+1))
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
        add(format("%s", tostring(value)))
    end
    return concat(output, '\n')
end
