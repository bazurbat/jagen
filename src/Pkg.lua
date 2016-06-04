local system = require 'system'
local Target = require 'Target'

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

function Pkg:set(key, value, config)
    if config then
        self.configs = self.configs or {}
        self.configs[config] = self.configs[config] or {}
        self.configs[config][key] = value
    else
        self[key] = value
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

function Pkg:add_req(req, config, template)
    local name, config = nil, config
    if type(req) == 'string' then
        name = req
    else
        name   = req[1]
        config = req[2] or config
    end

    Pkg:add {
        name = name,
        config = config,
        template = template
    }

    return { name = name, config = config }
end

function Pkg:add(rule)
    rule = Pkg:new(rule)

    local pkg = self.all[rule.name]

    if not pkg then
        pkg = Pkg:new { rule.name }

        for stage in each(self.init_stages) do
            pkg:add_target(Target:new(rule.name, stage))
        end

        table.merge(pkg, Pkg:new(assert(require('pkg/'..rule.name))))

        self.all[rule.name] = pkg
    end

    if rule.template then
        rule = table.merge(copy(rule.template), rule)
    end

    local config = rule.config; rule.config = nil
    local requires = rule.requires; rule.requires = nil

    local template = rule.template or rule.pass_template
                     or pkg:get('template', config)
    rule.template, rule.pass_template = nil, nil

    pkg:set('template', template, config)

    local stages = table.imove({
            config   = config,
            template = template
        }, rule)

    table.merge(pkg, rule)

    do local build = pkg.build
        if build and config then
            if build.type == 'GNU' then
                if build.generate or build.autoreconf then
                    local autoreconf_t = Target:parse({ 'autoreconf',
                            { 'libtool', 'install', 'host' }
                        }, pkg.name)
                    pkg:add_target(autoreconf_t)
                    Pkg:add { 'libtool', 'host' }
                end
            end

            if build.type then
                local stages = {
                    { 'configure',
                        { 'toolchain', 'install', config }
                    },
                    { 'compile' },
                    { 'install' }
                }
                for _, stage in ipairs(stages) do
                    pkg:add_target(Target:parse(stage, pkg.name, config))
                end
                Pkg:add { 'toolchain', config }
            end
        end
    end

    if pkg.install and config then
        pkg:add_config(config)
        pkg.configs[config].install = pkg.install
        pkg.install = nil
    end

    -- add global stages specified in pkg file regardless of config or build
    for _, stage in ipairs(pkg) do
        pkg:add_target(Target:parse(stage, pkg.name, config))
    end

    for _, item in ipairs(requires or {}) do
        local req = Pkg:add_req(item, config, template)
        pkg:add_target(Target:parse({ 'configure',
                    { req.name, 'install', req.config }
            }, pkg.name, config))
    end

    -- evaluate requires for every add to collect rules from all templates
    for _, item in ipairs(pkg.requires or {}) do
        local req = Pkg:add_req(item, config, template)
        pkg:add_target(Target:parse({ 'configure',
                    { req.name, 'install', req.config }
                }, pkg.name, config))
    end

    for _, stage in ipairs(stages) do
        pkg:add_target(Target:parse(stage, pkg.name, config))
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

function Pkg:build_dirs(config)
    local o = {}
    local function get_dir(config)
        return system.pread('*l',
            'jagen-pkg -q build_dir %s %s', self.name, config)
    end
    if config then
        if not self:has_config(config) then
            jagen.die("package '%s' does not have config: %s", self.name, config)
        end
        o[config] = assert(get_dir(config))
    elseif self.configs then
        for k, v in pairs(self.configs) do
            o[k] = assert(get_dir(k))
        end
    end
    return o
end

return Pkg
