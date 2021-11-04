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
        configs = {},
        packages  = {},
        templates = {},
        named_templates = {},
        parse_templates = {},
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

function Module.env.config(rule)
    append(current.configs, Config:new(rule))
end

function Module.env.package(rule)
    -- Log.debug2('package: %s', pretty(rule))
    local pkg = Package:from_rule(rule)
    append(current.packages, pkg)
end

function Module.env.template(rule)
    -- Log.debug2('template: %s', pretty(rule))
    if rule.name then
        append(current.named_templates, rule)
    elseif rule.parse then
        append(current.parse_templates, rule)
    elseif rule.final then
        append(current.final_templates, rule)
    else
        append(current.templates, rule)
    end
end

function Module.env.bind(...)
    local fns = {...}
    return function(init, state)
        local result = init
        for i = 1, #fns do
            result = fns[i](result, state)
        end
        return result
    end
end

function Module.env.cat(...)
    local args = {...}
    return function(_, state)
        local result, value = {}
        for i = 1, #args do
            local arg = args[i]
            if type(arg) == 'function' then
                value = arg(nil, state)
            else
                value = tostring(arg)
            end
            append(result, value)
        end
        return table.concat(result)
    end
end

function Module.env.none(value, state)
    if state.matching then
        return value == nil
    end
end

function Module.env.some(value, state)
    return value ~= nil
end

function Module.env.as(name)
    name = name or true
    return function(value, state)
        if not state.value[name] then
            state.value[name] = value
            return value
        else
            return state.value[name]
        end
    end
end

function Module.env.value(name, state)
    local key = true
    local function this(value, state)
        if state.matching then
            state.value[key] = value
            return value
        else
            return state.value[key]
        end
    end
    if state ~= nil then
        return this(name, state)
    else
        key = name
        return this
    end
end

function Module.env.anyof(...)
    local args = { ... }
    return function(value, state)
        if state.matching then
            for i = 1, #args do
                if Rule:match(value, args[i], state) then
                    return true
                end
            end
        else
            for i = 1, #args do
                local arg = args[i]
                if type(arg) == 'function' then
                    arg = arg(value, state)
                end
                if arg then
                    return arg
                end
            end
        end
    end
end

function Module.env.oftype(typename)
    return function(value, state)
        return type(value) == typename
    end
end

function Module.env.contains(item)
    return function(pvalue, self)
        if type(pvalue) == 'table' then
            for _, value in ipairs(pvalue) do
                if value == item then
                    return true
                end
            end
        else
            return pvalue == item
        end
    end
end

function Module.env.match(pattern)
    return function (value, state)
        return string.match(value, pattern)
    end
end

function Module.env.each(values, state)
    if not state.each then
        state.i = 0
        state.n = values and #values or 0
        state.values = values
        state.each = true
        return true
    else
        return state.values[state.i]
    end
end

function Module.env.from(ref, key)
    return function(value, state)
        local this_ref
        if type(ref) == 'function' then
            this_ref = ref(nil, state)
        end
        return table.get(state.packages, this_ref, unpack(string.split2(key, '.')))
    end
end

function Module.env.as_target(spec, state)
    local name, config = table.unpack(string.split2(spec, ':'))
    return { name = name, config = config }
end

function Module.env.with_stage(stage)
    return function(target, state)
        target.stage = stage
        return target
    end
end

function Module.env.target(name, stage, config)
    -- return Target.from_args(name, stage, config)
end

function Module.env.stage(name)
    return { stage = name }
end

return Module
