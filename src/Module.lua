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

function Module:collect_uses(result)
    for mod in each(self.uses) do
        append(result, mod)
        if mod.uses then
            mod:collect_uses(result)
        end
    end
    return result
end

function Module:load(name, filename)
    Log.debug2("load module '%s' (%s)", name, filename)

    local this = Module:new(name)

    current = this
    depth = depth + 1
    this:_loadfile(filename)
    depth = depth - 1
    current = nil

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
        Log.debug2("try load package '%s': file not found in %s", target.ref, searchpath)
        return nil, err
    end

    local module = Module.loaded[filename]

    Log.debug2("load package '%s' from '%s'%s", target.ref, filename,
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

    Log.debug2("use module '%s' (%s) [%d] from '%s' %s", name, filename, depth, current.name,
               this and '(already loaded)' or '')

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
    -- Log.debug2('package: %s', pretty(rule))
    local pkg = Package:from_rule(rule)
    append(current.packages, pkg)
end

function Module.env.template(rule)
    -- Log.debug2('template: %s', pretty(rule))
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
    if state.matching then
        return value == nil
    end
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
    end
    if type(state) == 'string' then
        return impl
    else
        return impl(state, value)
    end
end

function Module.env.anyof(args)
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

function Module.env.contains(pattern)
    return function(state, value)
        if type(value) == 'table' then
            for _, v in ipairs(value) do
                if string.match(v, pattern) then
                    return true
                end
            end
        elseif type(value) == 'string' then
            return string.match(value, pattern)
        end
    end
end

function Module.env.match(pattern)
    return function (state, value)
        return string.match(value, pattern)
    end
end

function Module.env.each(state, value)
    if state.matching then
        state.i = 0
        state.n = value and #value or 0
        state.values = value
        state.each = true
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

return Module
