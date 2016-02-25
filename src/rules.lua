require 'Target'

local system = require 'system'

local mkpath = system.mkpath

local P = {}

local Rule = {
    init_stages = { 'unpack', 'patch' }
}
Rule.__index = Rule

local packages = {}

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
        table.insert(o, Rule:new(rule, template))
    end
    local chunk = loadfile(filename)
    if chunk then
        setfenv(chunk, env)
        chunk()
    end
    return o
end

local function add_package(rule)
    local key = tostring(rule)
    local pkg = packages[key]

    if not pkg then
        pkg = Rule:new_package { rule.name }

        table.merge(pkg, rule)

        if pkg.build and pkg.config then
            pkg:add_build_stages()
        end

        pkg:add_stages(pkg)

        if pkg.source and pkg.source.type == 'repo' then
            local unpack = {
                { 'unpack', requires = { { 'repo', 'host' } } }
            }
            pkg:add_stages(unpack)
        end

        packages[key] = pkg
    else
        table.merge(pkg, rule)

        if pkg.build and pkg.config then
            pkg:add_build_stages()
            pkg:add_stages(pkg)
        end
    end

    pkg:add_stages(rule)
end

function Rule:__tostring()
    return string.format('%s__%s', self.name or '', self.config or '')
end

function Rule:parse(rule)
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

function Rule:new(rule, template)
    rule = Rule:parse(rule)
    local new
    if template then
        new = Rule:parse(copy(template))
        table.merge(new, rule)
        new.template = template
    else
        new = rule
    end
    setmetatable(new, self)
    return new
end

function Rule:new_package(rule)
    local pkg  = Rule:new(rule)
    local name = pkg.name

    pkg.stages = pkg.stages or {}

    for stage in each(self.init_stages) do
        pkg:add_target(Target:new(name, stage))
    end

    for filename in each(import_paths('pkg/'..name..'.lua')) do
        table.merge(pkg, Rule:new(loadsingle(filename)))
    end

    return pkg
end

function Rule:add_stages(rule)
    local template = rule.template or {}

    for i, stage in ipairs(rule) do
        self:add_stage(stage, template)
 
        rule[i] = nil
    end
end

function Rule:add_target(target)
    local name   = target.stage
    local config = target.config
    local shared = {
        unpack = true,
        patch  = true,
    }

    local function add_to(pkg)
        if not pkg.stages then
            pkg.stages = {}
        end
        local stages = pkg.stages
        if stages[name] then
            stages[name]:add_inputs(target)
        else
            table.insert(stages, target)
            stages[name] = target
        end
    end

    if not config or shared[name] then
        add_to(self)
    else
        if not self.configs then
            self.configs = {}
        end
        if not self.configs[config] then
            self.configs[config] = {}
        end

        add_to(self.configs[config])
    end

    return self
end

function Rule:add_stage(stage, template)
    local config = self.config or template and template.config
    local tc = not stage.shared and self.config
    local target = Target:parse(stage, self.name, tc)

    for _, item in ipairs(stage.requires or {}) do
        local config, name = config
        if type(item) == 'string' then
            name = item
        else
            name   = item[1]
            config = item[2] or config
        end

        target:append(Target:required(name, config))
        add_package(Rule:new({ name = name, config = config }, template))
    end

    self:add_target(target)
end

function Rule:add_build_stages()
    local build = self.build
    local stages = {}

    if self.requires then
        table.insert(self, { 'configure', requires = self.requires })
    end

    if build.type == 'GNU' then
        if build.generate or build.autoreconf then
            local autoreconf = { 'autoreconf', shared = true,
                requires = { { 'libtool', 'host' } }
            }

            table.insert(stages, autoreconf)
        end
    end

    if build.type then
        table.insert(stages, { 'configure', requires = { 'toolchain' } })
        table.insert(stages, { 'compile' })
        table.insert(stages, { 'install' })
    end

    for stage in each(stages) do
        self:add_stage(stage)
    end
end

function Rule:add_ordering_dependencies()
    local prev, common

    for s in self:each() do
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
    return coroutine.wrap(function ()
            for _, t in ipairs(self.stages) do
                coroutine.yield(t)
            end
            for k, c in pairs(self.configs or {}) do
                for _, t in ipairs(c.stages or {}) do
                    coroutine.yield(t)
                end
            end
        end)
end

function P.load()
    local Source = require 'Source'

    for filename in each(import_paths('rules.lua')) do
        for rule in each(loadall(filename)) do
            add_package(rule)
        end
    end

    for _, pkg in pairs(packages) do
        pkg.source = Source:create(pkg.source, pkg.name)
    end

    return packages
end

function P.merge(rules)
    local list = {}

    for _, rule in pairs(rules) do
        local name = assert(rule.name)
        local pkg = list[name]
        if pkg then
            for target in rule:each() do
                pkg:add_target(target)
            end
            -- FIXME: really need to sanitize rule handling to get rid of merge
            -- step
            if pkg.source and rule.source then
                table.merge(pkg.source, rule.source)
            end
        else
            list[name] = rule
        end
    end

    return list
end

return P
