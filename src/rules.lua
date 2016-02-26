require 'Target'

local system = require 'system'

local mkpath = system.mkpath

local P = {}
P.level = 0

function P.indent()
    return string.rep('  ', P.level)
end

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

function Rule:collect_stages()
    local o = {}
    for i, v in ipairs(self) do
        table.insert(o, v)
        self[i] = nil
    end
    return o
end

function Rule:merge(rule)
    -- do not append the same template again and again, just replace it
    if rule.template then
        self.template = nil
    end
    table.merge(self, rule)
    return self
end

function Rule:has_config(name)
    return self.configs and self.configs[name]
end

function Rule:add_config(name)
    if not self.configs then
        self.configs = {}
    end
    if not self.configs[name] then
        self.configs[name] = {}
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

function Rule:add_stage(stage, template, config)
    local config = config or self.config or template and template.config
    local tc = not stage.shared and self.config
    local target = Target:parse(stage, self.name, tc)

    jagen.debug2('%s| %s', P.indent(),
        Target.__tostring(target, ':'))

    for _, item in ipairs(stage.requires or {}) do
        local config, name = config
        if type(item) == 'string' then
            name = item
        else
            name   = item[1]
            config = item[2] or config
        end

        target:append(Target:required(name, config))
        P.level = P.level + 1
        Rule:add_package(Rule:new({ name = name, config = config }, template))
        P.level = P.level - 1
    end

    self:add_target(target)
end

function Rule:add_package(rule)
    local key = rule.name
    local pkg = packages[key]
    local config = rule.config

    local rule_stages = rule:collect_stages()

    jagen.debug2('%s+ %s %s', P.indent(),
        rule.name, rule.config or '')

    if not pkg then
        pkg = Rule:new_package { rule.name }
        packages[key] = pkg
    end

    pkg:merge(rule)

    local source = pkg.source

    if source and source.type == 'repo' then
        local unpack = { 'unpack', requires = { { 'repo', 'host' } } }
        pkg:add_stage(unpack)
    end

    local build  = pkg.build

    if build and config and not pkg:has_config(config) then
        pkg:add_config(config)

        if pkg.requires then
            append(pkg, { 'configure', requires = pkg.requires })
        end

        if build.type == 'GNU' then
            if build.generate or build.autoreconf then
                local autoreconf = { 'autoreconf', shared = true,
                    requires = { { 'libtool', 'host' } }
                }
                pkg:add_stage(autoreconf)
            end
        end

        if build.type then
            local build_stages = {
                { 'configure',
                    requires = { 'toolchain' }
                },
                { 'compile' },
                { 'install' }
            }
            for i, stage in ipairs(build_stages) do
                pkg:add_stage(stage)
            end
        end
    end

    if pkg.install then
        pkg.configs[config].install = pkg.install
        pkg.install = nil
    end

    for i, stage in ipairs(pkg) do
        pkg:add_stage(stage, pkg.template, config)
    end

    for _, stage in ipairs(rule_stages) do
        pkg:add_stage(stage, pkg.template, config)
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
            Rule:add_package(rule)
        end
    end

    for _, pkg in pairs(packages) do
        pkg.source = Source:create(pkg.source, pkg.name)
    end

    return packages
end

return P
