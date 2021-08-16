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
    if type(rule) == 'string' then
        rule = { name = rule }
    elseif type(rule) == 'table' then
        if type(rule[1]) == 'string' then
            rule.name = rule[1]
            table.remove(rule, 1)
        end
        if type(rule[1]) == 'string' then
            rule.config = rule[1]
            table.remove(rule, 1)
        end
    else
        error("invalid rule type")
    end

    rule.ref = Package.__tostring(rule)

    rule.source = Source:parse(rule.source)

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

return Package
