local Rule   = require 'Rule'
local Source = require 'Source'

local Package = Rule:new()

function Package:new(name, config)
    local pkg = { name = assert(name), config = config }
    pkg.ref = string.format('%s%s', name, config and ':'..config or '')
    setmetatable(pkg, self)
    self.__index = self
    return pkg
end

function Package:from_rule(rule)
    rule = Package:parse(rule)
    setmetatable(rule, self)
    self.__index = self
    return rule
end

function Package:__tostring(sep)
    local c = {}
    if self.name then table.insert(c, self.name) end
    if self.config then table.insert(c, self.config) end
    return table.concat(c, sep or ':')
end

function Package:create(name)
    local pkg = {
        name     = name,
    }
    setmetatable(pkg, self)
    return pkg
end

function Package:parse(rule)
    rule.ref = rule.name
    rule.source = Source:parse(rule.source)

    for key in each { 'class', 'uses' } do
        local value = rule[key]
        if type(value) == 'string' then
            rule[key] = { value }
        end
    end

    for key in each { 'build', 'install' } do
        local value = rule[key]
        if type(value) == 'string' then
            rule[key] = { type = value }
        end
    end

    if type(rule.patches) == 'table' then
        local patches = rule.patches
        for i = 1, #patches do
            local item = patches[i]
            if type(item) == 'string' then
                patches[i] = { item, 1 }
            elseif type(item) == 'table' then
                if not item[2] then
                    item[2] = 1
                end
            end
        end
    end

    if type(rule.files) == 'string' then
        rule.files = { rule.files }
    end
    if type(rule.files) == 'table' then
        local files = rule.files
        for i = 1, #files do
            local item = files[i]
            if type(item) == 'string' then
                files[i] = { item }
            elseif type(item) == 'table' then
                if not item.path and item.dir then
                    item.path = item.dir..'/'..item[1]
                end
            end
        end
    end

    return rule
end

local function collect_array(array, result)
    result = result or {}
    for key, value in ipairs(array) do
        local tvalue = type(value)
        if tvalue == 'string' then
            append(result, value)
        elseif tvalue == 'number' then
            append(result, tostring(value))
        elseif tvalue == 'table' then
            collect_array(value, result)
        end
    end
    return result
end

local function format_table(path, value, fmt)
    local tvalue = type(value)
    if tvalue == 'string' then
        fmt(path, value)
    elseif tvalue == 'number' then
        fmt(path, tostring(value))
    elseif tvalue == 'table' then
        for k, v in kvpairs(value) do
            format_table(append(path, k), v, fmt)
        end
        if #value > 0 then
            fmt(path, table.concat(collect_array(value), '\t'))
        end
    end
end

local function format_property(property, prefix, skip)
    skip = skip or {}

    local format = string.format
    local result = {}

    local function format_variable(path, value)
        local name = string.to_identifier(table.concat(path, '_'))
        if value == '<unset>' then
            append(result, format('unset %s', name))
        else
            append(result, format("export %s='%s'", name, value))
        end
    end

    for key, value in pairs(property) do
        if not skip[key] then
            local path = prefix and {prefix, key} or {key}
            format_table(path, value, format_variable)
        end
    end

    table.sort(result)

    return table.concat(result, '\n')
end

function Package:generate_export_script()
    local result = {}
    if self.export then
        append(result, format_property(self.export, self.name, { env = true }))
        if self.export.env then
            append(result, format_property(self.export.env))
        end
    end
    return table.concat(result, '\n')
end

return Package
