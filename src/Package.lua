local System = require 'System'
local Target = require 'Target'
local Source = require 'Source'

local Package = {
    init_stages = { 'unpack', 'patch' }
}
Package.__index = Package

local packages = {}

function Package:__tostring()
    return string.format('%s__%s', self.name or '', self.config or '')
end

function Package:parse(rule, config)
    if type(rule) == 'string' then
        return { name = rule, config = config }
    end

    if type(rule[1]) == 'string' then
        rule.name = rule[1]
        table.remove(rule, 1)
    end

    if type(rule[1]) == 'string' then
        rule.config = rule[1]
        table.remove(rule, 1)
    end
    rule.config = rule.config or config

    if type(rule.source) == 'string' then
        if string.match(rule.source, '^https?://') then
            rule.source = { type = 'curl', location = rule.source }
        else
            rule.source = { type = 'dist', location = rule.source }
        end
    end
    return rule
end

function Package:new(rule)
    rule = Package:parse(rule)
    setmetatable(rule, self)
    return rule
end

function Package:has_config(name)
    return self.configs and self.configs[name]
end

function Package:add_config(name)
    if not self.configs then
        self.configs = {}
    end
    if not self.configs[name] then
        self.configs[name] = {}
    end
end

function Package:get(key, config)
    if config and self.configs and self.configs[config] then
        return self.configs[config][key]
    else
        return self[key]
    end
end

function Package:set(key, value, config)
    if config then
        self.configs = self.configs or {}
        self.configs[config] = self.configs[config] or {}
        self.configs[config][key] = value
    else
        self[key] = value
    end
end

function Package:add_target(rule, config)
    local target = Target:parse(rule, self.name, config)
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

function Package:add_ordering_dependencies()
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

function Package:each()
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

function Package:build_dirs(config)
    local o = {}
    local function get_dir(config)
        return System.pread('*l',
            'jagen-pkg -q build_dir %s %s', self.name, config)
    end
    if config then
        assert(self:has_config(config))
        o[config] = assert(get_dir(config))
    elseif self.configs then
        for k, v in pairs(self.configs) do
            o[k] = assert(get_dir(k))
        end
    end
    return o
end

function Package.load_rules(full)
    local env = { Package = Package }
    setmetatable(env, { __index = _G })
    local dirs = System.getenv { 'jagen_product_dir', 'jagen_root' }
    for _, dir in ipairs(dirs) do
        local filename = dir..'/rules.lua'
        if System.file_exists(filename) then
            local chunk = assert(loadfile(filename))
            setfenv(chunk, env)
            chunk()
        end
    end

    for _, pkg in pairs(packages) do
        pkg.source = Source:create(pkg.source, pkg.name)
        if full then
            pkg:add_ordering_dependencies()
        end
    end

    return packages
end

function define_rule(rule)
    rule = Package:new(rule)

    local pkg = packages[rule.name]

    if not pkg then
        pkg = Package:new { rule.name }

        for stage in each(Package.init_stages) do
            pkg:add_target { stage }
        end

        table.merge(pkg, Package:new(assert(require('pkg/'..rule.name))))

        packages[rule.name] = pkg
    end

    if rule.template then
        rule = table.merge(copy(rule.template), rule)
    end

    local config = rule.config; rule.config = nil
    local requires = rule.requires or {}; rule.requires = nil
    local install = rule.install or {}; rule.install = nil

    local template = rule.template or rule.pass_template
                     or pkg:get('template', config)
    rule.template, rule.pass_template = nil, nil

    pkg:set('template', template, config)

    local stages = table.imove({}, rule)

    table.merge(pkg, rule)

    if pkg.source.type == 'repo' then
        pkg:add_target { 'unpack',
            { 'repo', 'install', 'host' }
        }
        define_rule { 'repo', 'host' }
    end

    do local build = pkg.build
        if build and config then
            if build.type == 'GNU' then
                if build.generate or build.autoreconf then
                    pkg:add_target { 'autoreconf',
                        { 'libtool', 'install', 'host' }
                    }
                    define_rule { 'libtool', 'host' }
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
                    pkg:add_target(stage, config)
                end
                define_rule { 'toolchain', config }
            end
        end
    end

    if install and config then
        local existing = pkg:get('install', config) or {}
        local install = table.merge(existing, install)

        if pkg.install and pkg.install.modules or install.modules then
            pkg:add_target({ 'install_modules' }, config)
        end

        pkg:set('install', install, config)
    end

    -- add global stages specified in pkg file regardless of config or build
    for _, stage in ipairs(pkg) do
        pkg:add_target(stage, config)
    end

    -- evaluate pkg requires for every add to collect rules from all templates
    table.iextend(requires, pkg.requires or {})

    for _, item in ipairs(requires) do
        local req = Package:parse(item, config)
        pkg:add_target({ 'configure',
                { req.name, 'install', req.config }
            }, config)
        define_rule {
            name = req.name,
            config = req.config,
            template = template
        }
    end

    for _, stage in ipairs(stages) do
        for _, item in ipairs(stage.requires or {}) do
            local req = Package:parse(item, config)
            table.insert(stage, { req.name, 'install', req.config })
            define_rule {
                name = req.name,
                config = req.config,
                template = template
            }
        end
        pkg:add_target(stage, config)
    end
end

return Package
