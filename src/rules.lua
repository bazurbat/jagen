require 'Target'
local system = require 'system'

local mkpath = system.mkpath

local function import_paths(filename)
    local o = {}
    table.insert(o, mkpath(jagen.dir, 'lib', filename))
    for _, overlay in ipairs(string.split(jagen.overlays, ' ')) do
        table.insert(o, mkpath(jagen.dir, 'overlay', overlay, filename))
    end
    table.insert(o, mkpath(jagen.root, filename))
    return o
end

local function loadsingle(filename)
    local o, env = {}, {}
    function env.package(rule)
        o = rule
    end
    local chunk = loadfile(filename)
    if chunk then
        setfenv(chunk, env)
        chunk()
    end
    return o
end

local function loadall(filename)
    local o, env = {}, {
        table = table,
        jagen = jagen
    }
    function env.package(rule)
        table.insert(o, rule)
    end
    local chunk = loadfile(filename)
    if chunk then
        setfenv(chunk, env)
        chunk()
    end
    return o
end

local Rule = {
    init_stages = { 'unpack', 'patch' }
}
Rule.__index = Rule

function Rule:qname()
    local name = assert(self.name)
    local config = self.config
    if config then
        return name..'-'..config
    else
        return name
    end
end

function Rule:read(rule)
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
    return rule
end

function Rule:create(name)
    local pkg = { name = name, stages = {} }
    setmetatable(pkg, self)
    self.__index = self

    for _, s in ipairs(self.init_stages) do
        pkg:add_target(Target:new(name, s))
    end

    for filename in each(import_paths('pkg/'..name..'.lua')) do
        table.merge(pkg, Rule:read(loadsingle(filename)))
    end

    return pkg
end

function Rule:add_target(target)
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

function Rule:add_build_targets(config)
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

function Rule:add_ordering_dependencies()
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

local P = {}

function P.load()
    local rules = {}

    local function add(rule)
        rule = Rule:read(rule)
        local name = assert(rule.name)
        local qname = Rule.qname(rule)
        local pkg = rules[qname]
        if not pkg then
            pkg = Rule:create(name)
            rules[qname] = pkg
        end
        table.merge(pkg, rule)
        pkg:add_build_targets(rule.config)
        for stage in each(rule) do
            pkg:add_target(Target:from_rule(stage, pkg.name, rule.config))
        end
    end

    for filename in each(import_paths('rules.lua')) do
        for rule in each(loadall(filename)) do
            add(rule)
        end
    end

    add { 'toolchain', 'target', { 'install' } }

    if jagen.need_libtool then
        add { 'libtool', 'host' }
    end

    if jagen.need_repo then
        add { 'repo' }
    end

    for _, pkg in pairs(rules) do
        pkg.source = Source:create(pkg.source)
    end

    return rules
end

function P.merge(rules)
    local packages = {}
    for qname, rule in pairs(rules) do
        local name = assert(rule.name)
        local pkg = packages[name]
        if pkg then
            for target in each(rule.stages) do
                pkg:add_target(target)
            end
        else
            packages[rule.name] = rule
            table.insert(packages, rule)
        end

        local filename = mkpath(jagen.include_dir, rule:qname()..'.sh')
        local file = assert(io.open(filename, 'w+'))
        Script:write(rule, file)
        file:close()
    end
    return packages
end

return P
