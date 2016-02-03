require 'Target'
local system = require 'system'

local mkpath = system.mkpath

local P = {}

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
    function env.package(rule, template)
        if template then
            table.merge(rule, template)
            rule.template = template
        end
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

function Rule:__tostring()
    local name = self.name
    local config = self.config
    if name and config then
        return string.format('%s__%s', name, config)
    else
        return name or config or 'rule'
    end
end

function Rule:parse(rule)
    setmetatable(rule, self)
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

function Rule:new_package(rule)
    local pkg  = Rule:parse(rule)
    local name = pkg.name

    pkg.stages = pkg.stages or {}

    for stage in each(self.init_stages) do
        pkg:add_target(Target:new(name, stage))
    end

    for filename in each(import_paths('pkg/'..name..'.lua')) do
        table.merge(pkg, Rule:parse(loadsingle(filename)))
    end

    return pkg
end

function Rule:add_package(rule, list)
    rule = Rule:parse(rule)

    local key = tostring(rule)
    local pkg = list[key]

    if not pkg then
        pkg = Rule:new_package { rule.name, rule.config }

        table.merge(pkg, rule)

        pkg:add_default_targets(list)
        pkg:add_targets(pkg, list)

        list[key] = pkg
    else
        table.merge(pkg, rule)
    end

    pkg:add_targets(rule, list)
end

function Rule:add_targets(rule, list)
    local template = rule.template or {}
    local config = template.config or self.config

    for _, stage in ipairs(rule) do
        local target = Target:from_rule(stage, self.name, self.config)

        for _, name in ipairs(stage.requires or {}) do
            local req_input = Target:new(name, 'install', config)
            table.insert(target.inputs, req_input)

            local req_rule = {
                name = name,
                config = config,
            }
            table.merge(req_rule, template)
            req_rule.template = template

            Rule:add_package(req_rule, list)
        end

        self:add_target(target)
    end
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

function Rule:add_default_targets(list)
    local source = self.source
    local build = self.build
    local config = self.config

    if source then
        if source.type == 'repo' then
            P.need_repo = true
            self:add_target(Target:from_rule({ 'unpack',
                        { 'repo', 'unpack' }
                }, self.name))
        end
    end

    if self.requires then
        table.insert(self, { 'configure', requires = self.requires })
    end

    if build then
        if build.type == 'GNU' then
            if build.generate or build.autoreconf then
                P.need_libtool = true
                self:add_target(Target:from_rule({ 'autoreconf',
                            { 'libtool', 'install', 'host' }
                    }, self.name))
            end
        end
        if build.type then
            local build_rules = {
                { 'configure', requires = { 'toolchain' } },
                { 'compile' },
                { 'install' }
            }
            self:add_targets(build_rules, list)
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

function P.load()
    local packages = {}
    local source = require 'sources'

    local function add(rule)
        Rule:add_package(rule, packages)
    end

    for filename in each(import_paths('rules.lua')) do
        for rule in each(loadall(filename)) do
            add(rule)
        end
    end

    if P.need_libtool then
        add { 'libtool', 'host' }
    end

    if P.need_repo then
        add { 'repo' }
    end

    for _, pkg in pairs(packages) do
        pkg.source = source:create(pkg.source)
    end

    return packages
end

function P.merge(rules)
    local packages = {}

    for _, rule in pairs(rules) do
        local name = assert(rule.name)
        local pkg = packages[name]
        if pkg then
            for target in each(rule.stages) do
                pkg:add_target(target)
            end
        else
            packages[name] = rule
        end
    end

    return packages
end

return P
