local Config   = require 'Config'
local Log      = require 'Log'
local Package  = require 'Package'
local Rule     = require 'Rule'
local System   = require 'System'

local Module = {
    env = {},
    loaded = {}
}

setmetatable(Module.env, {__index = _G})

local current
local depth = 0

function Module:new(name)
    local module = {
        name = name,
        uses = {},
        packages  = {},
        templates = {},
        final_templates = {},
    }
    setmetatable(module, self)
    self.__index = self
    return module
end

function Module:__tostring()
    return string.format('%s (%s)', self.name, self.filename)
end

function Module:collect_unprocessed(processed)
    local result, seen = {}, {}
    local function add(mod)
        if not processed[mod.filename] and not seen[mod.filename] then
            append(result, mod)
            seen[mod.filename] = mod
        end
    end
    local function collect(mod)
        for use in each(mod.uses) do
            if use.uses then
                collect(use)
            end
            add(use)
        end
    end
    collect(self)
    add(self)
    return result
end

function Module:load(name, filename)
    Log.debug1("load module '%s' (%s)", name, filename)

    local this = Module:new(name)

    current = this
    depth = depth + 1
    this:_loadfile(filename)
    depth = depth - 1
    current = nil

    Log.debug1("end load module '%s': %d packages, %d templates",
        name, #this.packages, #this.templates)

    return this
end

function Module:load_package(target, dirlist)
    local name, config = assert(target.name), target.config
    local path = {}
    for dir in each(dirlist) do
        prepend(path, string.format('%s/pkg/?.lua', dir, name))
    end
    local searchpath = table.concat(path, ';')
    local filename, err = package.searchpath(name, searchpath, '')

    if not filename then
        Log.debug1("try load package '%s': file not found in %s", target.ref, searchpath)
        return nil, err
    end

    local module = Module.loaded[filename]

    Log.debug1("load package '%s' from '%s'%s", target.ref, filename,
        module and ' (already loaded as '..module.name..')' or '')

    if not module then
        module = Module:new(target.ref)
        current = module
        depth = depth + 1
        module:_loadfile(filename)
        depth = depth - 1
        current = nil
    end

    return module
end

function Module:_loadfile(filename)
    local func, err = assert(loadfile(filename, 'bt', self.env))
    if _VERSION == 'Lua 5.1' then
        setfenv(func, self.env)
    end
    func()
    self.filename = filename
    Module.loaded[filename] = self
end

function Module:basename(path)
    return string.match(path, '^(.+)/rules%.lua$') or
    string.match(path, '^(.+)%.lua')
end

function Module.env.use(name)
    local filename, err = package.searchpath(name, "?.lua;?/rules.lua")
    assert(filename, err)

    local this = Module.loaded[filename]

    Log.debug1("use module '%s' (%s) [%d] from '%s' %s", name, filename, depth, current.name,
               this and '(already loaded)' or '')

    if depth == 10 then
        error(string.format('module use depth has reached 10'))
    end

    if not this then
        this = Module:new(name)
        local prev = current
        current = this
        depth = depth + 1
        this:_loadfile(filename)
        depth = depth - 1
        current = prev
    end

    append(current.uses, this)
end

function Module.env.package(rule)
    -- Log.debug1('package: %s', pretty(rule))
    local pkg = Package:from_rule(rule)
    local state = { pkg = pkg }
    for key, value in pairs(pkg) do
        pkg[key] = Module.env.expand(value)(state)
    end
    append(current.packages, pkg)
end

function Module.env.template(rule)
    -- Log.debug1('template: %s', pretty(rule))
    rule = Rule:new(rule)
    if rule.final then
        append(current.final_templates, rule)
    else
        append(current.templates, rule)
    end
end

function Module.env.bind(fns)
    return function(state, value)
        local result = value
        for i = 1, #fns do
            result = fns[i](state, result)
        end
        return result
    end
end

function Module.env.replace(args)
    return function(state)
        local result = {}
        for i = 1, #args do
            local arg = args[i]
            if type(arg) == 'function' then
                arg = arg(state)
            end
            if type(arg) == 'table' then
                for j = 1, #arg do
                    table.insert(result, arg[j])
                end
            else
                table.insert(result, arg)
            end
        end
        return result
    end
end

function Module.env.join(...)
    local sep, args
    if select('#', ...) == 1 then
        sep = ' '
        args = select(1, ...)
    else
        sep  = select(1, ...)
        args = select(2, ...)
    end
    return function(state)
        local result, value = {}
        for i = 1, #args do
            local arg = args[i]
            if type(arg) == 'function' then
                value = Module.env.join(sep, { arg(state) })(state)
            elseif type(arg) == 'table' then
                value = Module.env.join(sep, arg)(state)
            else
                value = tostring(arg)
            end
            if value ~= nil then
                append(result, value)
            end
        end
        if next(result) then
            return table.concat(result, sep)
        end
    end
end

function Module.env.cat(args)
    return Module.env.join('', args)
end

function Module.env.path(args)
    return Module.env.join('/', args)
end

function Module.env.argument(name, value)
    return function(state)
        local name, value = name, value
        if type(name) == 'function' then
            name = name(state)
        end
        if type(value) == 'function' then
            value = value(state)
        end
        if value ~= nil then
            return string.format('%s="%s"', name, value)
        end
    end
end

function Module.env.nonempty(arg)
    return function(state)
        local targ = type(arg)
        if targ == 'string' and string.len(arg) > 0 then
            return arg
        elseif targ == 'table' and next(arg) then
            return arg
        elseif targ == 'function' then
            return Module.env.nonempty(arg(state))(state)
        end
    end
end

function Module.env.none(state, value)
    return value == nil
end

function Module.env.some(state, value)
    return value ~= nil
end

function Module.env.isnot(other)
    return function(state, value)
        return value ~= other
    end
end

function Module.env.value(state, value)
    local key = true
    if type(state) == 'string' then
        key = state
    end
    local function impl(state, value)
        if state.matching then
            state.value[key] = value
            return value
        else
            return state.value[key]
        end
        return nil
    end
    if type(state) == 'string' then
        return impl
    else
        return impl(state, value)
    end
end

function Module.env.optional(expr)
    return function(state, value)
        if state.matching then
            local match = Rule.match(value, expr, state)
            if value == nil then
                return true
            else
                return match
            end
        else
            if value == nil then
                local expr = expr
                while type(expr) == 'function' do
                    expr = expr(state)
                end
                return expr
            else
                return value
            end
        end
    end
end

function Module.env.anyof(...)
    local args
    if select('#', ...) == 1 then
        args = select(1, ...)
    else
        args = {...}
    end
    return function(state, value)
        if state.matching then
            for i = 1, #args do
                if Rule.match(value, args[i], state) then
                    return true
                end
            end
        else
            for i = 1, #args do
                local arg = args[i]
                if type(arg) == 'function' then
                    arg = arg(state, value)
                end
                if arg then
                    return arg
                end
            end
        end
    end
end

function Module.env.oftype(typename)
    return function(state, value)
        return type(value) == typename
    end
end

function Module.env.contains(expr)
    local function impl(state, value)
        if type(value) == 'string' then
            if type(expr) == 'function' then
                return expr(state, value)
            else
                return expr == value
            end
        elseif type(value) == 'table' then
            for _, v in ipairs(value) do
                local match = impl(state, v)
                if match then
                    return match
                end
            end
        end
    end
    return impl
end

function Module.env.match(pattern)
    return function (state, value)
        return string.match(value, pattern)
    end
end

function Module.env.each(state, value)
    if state.matching then
        state.n = value and #value or 0
        state.values = value
        state.each = true
        if state.debug then
            print(pretty(state.values))
        end
        return true
    else
        return state.values[state.i]
    end
end

function Module.env.from(expr, key)
    return function(state)
        local ref = expr
        if type(expr) == 'function' then
            ref = expr(state)
        end
        local pkg = state.packages[ref]
        if pkg then
            local tkey = type(key)
            if tkey == 'string' then
                return table.get(pkg, unpack(string.split2(key, '.')))
            elseif tkey == 'function' then
                return key(pkg, state)
            elseif tkey == 'nil' then
                return pkg
            end
        end
    end
end

function Module.env.stage(stage)
    return function(state, pkg)
        if pkg then
            if pkg.stages and pkg.stages[stage] then
                return { name = pkg.name, stage = stage }
            end
        else
            return { stage = stage }
        end
    end
end

function Module.env.expand(expr)
    return function(state)
        local function sub(path)
            local keys = path:split2('.')
            local value = table.get(state.pkg, table.unpack(keys))
            if type(value) == 'function' then
                value = Module.env.expand(value)(state)
            end
            return value or ''
        end
        local value = expr
        local count, depth, max_depth = 0, 0, 10
        repeat
            if type(value) == 'function' then
                value, count = value(state), 1
            elseif type(value) == 'string' then
                value, count = value:gsub('${([%w_][%w_.:]+)}', sub)
            else
                count = 0
            end
            depth = depth + 1
        until count == 0 or depth == max_depth
        if depth == max_depth then
            Log.warning("substitution depth limit %d reached while expanding "..
                "property %s, current value: %s", max_depth, expr, value)
        end
        return value
    end
end

function Module.env.default(defvalue)
    return function(state, value)
        return value or defvalue
    end
end

return Module
