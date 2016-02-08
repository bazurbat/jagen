require 'Target'

local system = require 'system'

local mkpath = system.mkpath

local P = {}

local Rule = {
    init_stages = { 'unpack', 'patch' }
}
Rule.__index = Rule

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
        local t
        if template then
            t = copy(template)
            table.merge(t, Rule:parse(rule))
            t.template = template
        else
            t = rule
        end
        table.insert(o, t)
    end
    local chunk = loadfile(filename)
    if chunk then
        setfenv(chunk, env)
        chunk()
    end
    return o
end

local function add_package(rule, list)
    rule = Rule:parse(rule)

    local key = tostring(rule)
    local pkg = list[key]

    if not pkg then
        pkg = Rule:new_package { rule.name, rule.config }

        table.merge(pkg, rule)

        if pkg.build and pkg.config then
            pkg:add_build_stages(list)
        end

        pkg:add_stages(pkg, list)

        list[key] = pkg
    else
        table.merge(pkg, rule)
    end

    pkg:add_stages(rule, list)
end

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

function Rule:required(name, config, template)
    local rule = copy(assert(template))
    rule.name = name
    rule.config = config
    rule.template = template
    return rule
end

function Rule:add_stages(rule, list)
    local template = rule.template or {}
    local config = self.config or template.config

    for _, stage in ipairs(rule) do
        local tc = not stage.shared and self.config
        local target = Target:from_rule(stage, self.name, tc)

        for _, item in ipairs(stage.requires or {}) do
            local config, name = config
            if type(item) == 'string' then
                name = item
            else
                name   = item[1]
                config = item[2] or config
            end

            target:append(Target:required(name, config))
            add_package(Rule:required(name, config, template), list)
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
        return this.stage == target.stage and this.config == target.config or
            default(this)
    end

    local found = find(eq, self.stages)
    if found then
        found:add_inputs(target)
    else
        table.insert(self.stages, target)
    end

    return self
end

function Rule:add_build_stages(list)
    local build = self.build

    if self.requires then
        table.insert(self, { 'configure', requires = self.requires })
    end

    if build.type == 'GNU' then
        if build.generate or build.autoreconf then
            local autoreconf = {
                { 'autoreconf', shared = true,
                    requires = { { 'libtool', 'host' } }
                }
            }

            self:add_stages(autoreconf, list)
        end
    end

    if build.type then
        local build_rules = {
            { 'configure', requires = { 'toolchain' } },
            { 'compile' },
            { 'install' }
        }

        self:add_stages(build_rules, list)
    end
end

function Rule:add_ordering_dependencies()
    local prev, common

    for _, s in ipairs(self.stages) do
        if prev then
            s.inputs = s.inputs or {}
            if common and s.config ~= prev.config then
                append(s.inputs, common)
            else
                append(s.inputs, prev)
            end
        end

        prev = s
        if not s.config then
            common = s
        end
    end
end

function Rule:each()
    return each(self.stages)
end

function P.load()
    local packages = {}
    local Source = require 'Source'

    local function add(rule)
        add_package(rule, packages)
    end

    for filename in each(import_paths('rules.lua')) do
        for rule in each(loadall(filename)) do
            add(rule)
        end
    end

    for _, pkg in pairs(packages) do
        pkg.source = Source:create(pkg.source)
    end

    return packages
end

function P.merge(rules)
    local packages = {}

    for _, rule in pairs(rules) do
        local name = assert(rule.name)
        local pkg = packages[name]
        if pkg then
            for target in rule:each() do
                pkg:add_target(target)
            end
        else
            packages[name] = rule
        end
    end

    return packages
end

return P
