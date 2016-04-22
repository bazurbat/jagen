local system = require 'system'
local Target = require 'Target'

local level = 0

local function indent()
    return string.rep('  ', level)
end

local Pkg = {
    all = {},
    init_stages = { 'unpack', 'patch' }
}
Pkg.__index = Pkg

function Pkg:__tostring()
    return string.format('%s__%s', self.name or '', self.config or '')
end

function Pkg:parse(rule)
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

function Pkg:new(rule)
    rule = Pkg:parse(rule)
    setmetatable(rule, self)
    return rule
end

function Pkg:new_package(rule)
    local pkg  = Pkg:new(rule)
    local name = pkg.name

    pkg.stages = pkg.stages or {}

    for stage in each(self.init_stages) do
        pkg:add_target(Target:new(name, stage))
    end

    table.merge(pkg, Pkg:new(assert(require('pkg/'..name))))

    return pkg
end

function Pkg:merge(rule)
    -- do not append the same template again and again, just replace it
    if rule.template then
        self.template = nil
    end
    table.merge(self, rule)
    return self
end

function Pkg:has_config(name)
    return self.configs and self.configs[name]
end

function Pkg:add_config(name)
    if not self.configs then
        self.configs = {}
    end
    if not self.configs[name] then
        self.configs[name] = {}
    end
end

function Pkg:get(key, config)
    if config and self.configs and self.configs[config] then
        return self.configs[config][key]
    else
        return self[key]
    end
end

function Pkg:add_target(target)
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

function Pkg:add_stages(stages)
    local config = stages.config
    local template = stages.template

    if not config and template then
        config = template.config
    end

    for stage in each(stages) do
        local target_config = not stage.shared and self.config
        local target = Target:parse(stage, self.name, target_config)

        jagen.debug2('%s| %s', indent(),
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

            level = level + 1
            Pkg:add {
                name = name,
                config = config,
                template = template
            }
            level = level - 1
        end

        for item in each(stage.depends or {}) do
            local stage = stage[1]
            target:append(Target:new(item, stage, config))
            level = level + 1
            Pkg:add {
                name = item,
                config = config,
                template = template,
                { 'deploy' }
            }
            level = level - 1
        end

        self:add_target(target)
    end

    return self
end

function Pkg:add(rule)
    rule = Pkg:new(rule)

    jagen.debug2('%s+ %s', indent(), rule.name)

    local pkg = self.all[rule.name]

    if not pkg then
        pkg = Pkg:new_package { rule.name }
        self.all[rule.name] = pkg
    end

    local final = pkg.final

    if not final then
        local template = rule.template

        if template then
            if not rule.skip_template then
                rule = table.merge(copy(template), rule)
            end
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

function Pkg:add_ordering_dependencies()
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

function Pkg:each()
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

return Pkg
