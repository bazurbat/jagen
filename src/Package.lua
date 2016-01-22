require 'Target'
local system = require 'system'

Package = {
    init_stages = { 'unpack', 'patch' }
}
Package.__index = Package

function Package:__tostring()
    local o = {}
    if self.name then table.insert(o, self.name) end
    if self.config then table.insert(o, self.config) end
    return table.concat(o, ':')
end

function Package:qname()
    local name = assert(self.name)
    local config = self.config
    if config then
        return name..'-'..config
    else
        return name
    end
end

function Package:read(rule)
    if type(rule[1]) == 'string' then
        rule.name = rule[1]
        table.remove(rule, 1)
    end
    if type(rule[1]) == 'string' then
        rule.config = rule[1]
        table.remove(rule, 1)
    end
    if type(rule.source) == 'string' then
        rule.source = { type = 'dist', location = rule.source }
    end
    setmetatable(rule, self)
    self.__index = self
    return rule
end

function Package:load(filename)
    local pkg = {}
    local env = {}
    function env.package(rule)
        pkg = Package:read(rule)
    end
    local chunk = loadfile(filename)
    if chunk then
        setfenv(chunk, env)
        chunk()
    end
    return pkg
end

function Package:create(name)
    local pkg = { name = name, stages = {} }
    setmetatable(pkg, self)
    self.__index = self

    for _, s in ipairs(self.init_stages) do
        pkg:add_target(Target:new(name, s))
    end

    for filename in each(system.import_paths('pkg/'..name..'.lua')) do
        table.merge(pkg, pkg:load(filename))
    end

    return pkg
end

function Package:add_target(target)
    local function default(this)
        for _, stage in ipairs(self.init_stages) do
            if stage == target.stage and stage == this.stage then
                return this
            end
        end
    end
    local function eq(this)
        return this == target or default(this)
    end

    local found = find(eq, self.stages)
    if found then
        jagen.debug2(tostring(self), '=', tostring(target))
        found:add_inputs(target)
        return self
    else
        jagen.debug2(tostring(self), '+', tostring(target))
        table.insert(self.stages, target)
    end

    return self
end

function Package:add_build_targets(config)
    local source = self.source
    local build = self.build
    if source then
        if source.type == 'repo' then
            jagen.need_repo = true
            self:add_target(Target:from_rule({ 'unpack',
                        { 'repo', 'unpack' }
                }, self.name))
        end
    end
    if build then
        if build.type == 'GNU' then
            if build.generate or build.autoreconf then
                jagen.need_libtool = true
                self:add_target(Target:from_rule({ 'autoreconf',
                            { 'libtool', 'install', 'host' }
                    }, self.name))
            end
        end
        if build.type then
            if config == 'target' then
                self:add_target(Target:from_rule({ 'build',
                            { 'toolchain', 'install', 'target' }
                    }, self.name, config))
            else
                self:add_target(Target:from_rule({ 'build',
                    }, self.name, config))
            end
            self:add_target(Target:from_rule({ 'install'
                }, self.name, config))
        end
    end
end

function Package:add_ordering_dependencies()
    local prev, common

    for _, s in ipairs(self.stages) do
        if prev then
            if common and s.config ~= prev.config then
                table.insert(s.inputs, 1, common)
            else
                table.insert(s.inputs, 1, prev)
            end
        end

        prev = s
        if not s.config then
            common = s
        end
    end
end
