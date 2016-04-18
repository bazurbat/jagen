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

function Rule:__tostring()
    return string.format('%s__%s', self.name or '', self.config or '')
end

function Rule:loadsingle(filename)
    local o, env = {}, {}
    setmetatable(env, { __index = _G })
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

function Rule:loadall(filename)
    local env = {}
    setmetatable(env, { __index = _G })
    function env.package(rule, template)
        Rule:add_package(Rule:new(rule, template))
    end
    local chunk = loadfile(filename)
    if chunk then
        setfenv(chunk, env)
        chunk()
    end
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
    rule._template = template
    setmetatable(rule, self)
    return rule
end

function Rule:new_package(rule)
    local pkg  = Rule:new(rule)
    local name = pkg.name

    pkg.stages = pkg.stages or {}

    for stage in each(self.init_stages) do
        pkg:add_target(Target:new(name, stage))
    end

    for filename in each(import_paths('pkg/'..name..'.lua')) do
        table.merge(pkg, Rule:new(Rule:loadsingle(filename)))
    end

    return pkg
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

function Rule:get(key, config)
    if config and self.configs and self.configs[config] then
        return self.configs[config][key]
    else
        return self[key]
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

function Rule:add_stages(stages)
    local config = stages.config
    local template = stages.template

    if not config and template then
        config = template.config
    end

    for stage in each(stages) do
        local target_config = not stage.shared and self.config
        local target = Target:parse(stage, self.name, target_config)

        jagen.debug2('%s| %s', P.indent(),
            Target.__tostring(target, ':'))

        for item in each(stage.requires or {}) do
            local name, config = nil, config

            if type(item) == 'string' then
                name = item
            else
                name   = item[1]
                config = item[2] or config
            end

            target:append(Target:new(name, 'install', config))

            P.level = P.level + 1
            Rule:add_package(Rule:new({ name = name, config = config }, template))
            P.level = P.level - 1
        end

        for item in each(stage.depends or {}) do
            local stage = stage[1]
            target:append(Target:new(item, stage, config))
            P.level = P.level + 1
            Rule:add_package(Rule:new({ name = item, config = config,
                        { 'deploy' }
                }, template))
            P.level = P.level - 1
        end

        self:add_target(target)
    end

    return self
end

function Rule:add_package(rule)
    jagen.debug2('%s+ %s', P.indent(), rule.name)

    local pkg = packages[rule.name]

    if not pkg then
        pkg = Rule:new_package { rule.name }
        packages[rule.name] = pkg
    end

    local final = pkg.final

    if not final then
        local template = rule._template

        if template then
            rule = table.merge(copy(template), rule)
            rule.template = template
        end
    end

    local config = rule.config
    local stages = table.imove(rule, {})

    pkg:merge(rule)

    do local source = pkg.source
        if source and source.type == 'repo' then
            pkg:add_stages {
                { 'unpack',
                    requires = { { 'repo', 'host' } }
                }
            }
        end
    end

    do local build = pkg.build
        if build and config and not pkg:has_config(config) then
            pkg:add_config(config)

            if build.type == 'GNU' then
                if build.generate or build.autoreconf then
                    pkg:add_stages {
                        { 'autoreconf', shared = true,
                            requires = { { 'libtool', 'host' } }
                        }
                    }
                end
            end

            if build.type then
                pkg:add_stages {
                    config = config,
                    template = rule.template,
                    { 'configure',
                        requires = { 'toolchain' }
                    },
                    { 'compile' },
                    { 'install' }
                }
            end
        end
    end

    if pkg.install and config then
        pkg:add_config(config)
        pkg.configs[config].install = pkg.install
        pkg.install = nil
    end

    -- add global stages specified in pkg file regardless of config or build
    pkg:add_stages(pkg)

    -- evaluate requires for every add to collect rules from all templates
    if pkg.requires then
        pkg:add_stages {
            config = config,
            template = rule.template,
            { 'configure',
                requires = pkg.requires
            }
        }
    end

    if not pkg.final then
        stages.config = config
        stages.template = pkg.template
        pkg:add_stages(stages)
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
        Rule:loadall(filename)
    end

    for _, pkg in pairs(packages) do
        pkg.source = Source:create(pkg.source, pkg.name)
    end

    return packages
end

return P
