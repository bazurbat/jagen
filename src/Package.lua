local Source = require 'Source'
local Target = require 'Target'
local System = require 'System'

local P = {}
P.__index = P

local find_module = (function ()
    local package = package
    return function (name)
        assert(name)
        local names, common_name = {}, name:match('([%w%p]+)~.*')
        if common_name then
            append(names, string.format('pkg/%s/%s', name, common_name))
            append(names, string.format('pkg/%s', name))
            append(names, string.format('pkg/%s/%s', common_name, common_name))
            append(names, string.format('pkg/%s', common_name))
        else
            append(names, string.format('pkg/%s/%s', name, name))
            append(names, string.format('pkg/%s', name))
        end

        for path in each(Jagen:path()) do
            for name in each(names) do
                local filename = System.mkpath(path, name..'.lua')
                local file = io.open(filename, 'rb')
                if file then
                    local module = assert(loadstring(assert(file:read('*a')), filename))
                    file:close()
                    return module, filename
                end
            end
        end
    end
end)()

function P:new(rule)
    rule = P:parse(rule)
    setmetatable(rule, self)
    return rule
end

function P:__tostring(sep)
    local c = {}
    if self.name then table.insert(c, self.name) end
    if self.config then table.insert(c, self.config) end
    return table.concat(c, sep or ':')
end

function P:create(name)
    local pkg = {
        name     = name,
        stages   = {},
        configs  = {},
        source   = {},
        build    = {},
        install  = {},
        export   = {},
        contexts = {},
        _collected_targets = {},
    }
    setmetatable(pkg, self)
    pkg:add_stage('clean')
    pkg:add_stage('unpack')
    pkg:add_stage('patch')
    return pkg
end

function P:from_library(name)
    return find_module(name)
end

function P:parse(rule)
    if type(rule) == 'string' then
        rule = { name = rule }
    elseif type(rule) == 'table' then
        if type(rule[1]) == 'string' then
            rule.name = rule[1]
            table.remove(rule, 1)
        end
        if type(rule[1]) == 'string' then
            rule.config = rule[1]
            table.remove(rule, 1)
        end
    else
        error("invalid rule type")
    end

    rule.source = Source:parse(rule.source)

    if type(rule.patches) == 'table' then
        local patches = rule.patches
        for i = 1, #patches do
            local item = patches[i]
            if type(item) == 'string' then
                patches[i] = { item, 1 }
            elseif type(item) == 'table' then
                if not item[2] then
                    item[2] = 1
                end
            end
        end
    end

    if type(rule.files) == 'string' then
        rule.files = { rule.files }
    end
    if type(rule.files) == 'table' then
        local files = rule.files
        for i = 1, #files do
            local item = files[i]
            if type(item) == 'string' then
                files[i] = { item }
            elseif type(item) == 'table' then
                if not item.path and item.dir then
                    item.path = item.dir..'/'..item[1]
                end
            end
        end
    end

    local function parse_section(name)
        if rule[name] ~= nil then
            if type(rule[name]) ~= 'table' then
                rule[name] = { type = rule[name] }
            end
            local field = rule[name]
            if type(field[1]) == 'string' then
                if field.type == nil then
                    field.type = field[1]
                end
                table.remove(field, 1)
            end
            if type(field.type) == 'string' then
                field.type = field.type:tocanon()
            end
        end
    end

    local function parse_spawn(rule)
        local spawn = rule['spawn']
        if not spawn then return end
        if type(spawn) == 'string' then
            spawn = { name = spawn }
            rule.spawn = spawn
        end
    end

    parse_section('build')
    parse_section('install')
    parse_spawn(rule)

    if rule.build then
        parse_spawn(rule.build)
    end
    if rule.install then
        parse_spawn(rule.install)
    end

    if rule.build and rule.build.system and rule.build.arch == nil then
        rule.build.arch = string.match(rule.build.system, '^(%w+)-?')
    end

    if rule.build and rule.build.clean ~= nil then
        local clean = rule.build.clean
        if type(clean) ~= 'table' then
            rule.build.clean = { clean }
        end
    end

    if type(rule.template) == 'string' then
        rule.template = { rule.template }
    end
    if rule.requires then
        if type(rule.requires.template) == 'string' then
            rule.requires.template = { rule.requires.template }
        end
    end
    for stage in each(rule) do
        if stage.requires and type(stage.requires.template) == 'string' then
            stage.requires.template = { stage.requires.template }
        end
    end

    if type(rule.requires) == 'string' then
        rule.requires = { rule.requires }
    end

    if type(rule.uses) == 'string' then
        rule.uses = { rule.uses }
    end

    return rule
end

function P:has_config(name)
    if name == nil then
        return true
    else
        return self.configs and self.configs[name]
    end
end

function P:each()
    return coroutine.wrap(function ()
            if self.stages then
                for target in each(self.stages) do
                    coroutine.yield(target, self)
                end
            end
            if self.configs then
                local configs = {}
                for config, this in pairs(self.configs) do
                    table.insert(configs, this)
                end
                table.sort(configs, function (a, b)
                        return (a.config or '') < (b.config or '')
                    end)
                for this in each(configs) do
                    if this.stages then
                        for target in each(this.stages) do
                            coroutine.yield(target, this)
                        end
                    end
                end
            end
        end)
end

function P:each_config(with_shared)
    return coroutine.wrap(function ()
            if with_shared then
                coroutine.yield(self)
            end
            if self.configs then
                for config, this in pairs(self.configs) do
                    coroutine.yield(this, config)
                end
            end
        end)
end

function P:is_scm()
    return self.source and Source:is_known(self.source.type)
end

function P:get(key, config)
    if config then
        if self.configs and self.configs[config] then
            return self.configs[config][key]
        end
    else
        return self[key]
    end
end

function P:set(key, value, config)
    if config then
        self.configs = self.configs or {}
        self.configs[config] = self.configs[config] or {}
        self.configs[config][key] = value
    else
        self[key] = value
    end
    return value
end

function P:get_build(key, config)
    return self:get('build', config)[key]
end

function P:get_toolchain_build(key, config, packages)
    local name = self:get('build', config).toolchain
    if name then
        local toolchain = packages[name]
        if toolchain then
            return toolchain:get_build(key, config)
        end
    end
end

function P:find_target(pattern)
    for target in self:each() do
        if target:match(pattern) then
            return target
        end
    end
end

function P:gettoolchain(config)
    local host_toolchain = 'system-native:host'
    local target_toolchain = os.getenv('jagen_target_toolchain')
    if target_toolchain and #target_toolchain == 0 then
        target_toolchain = nil
    end
    local build, toolchain = self:get('build', config)
    if build then
        if build.toolchain ~= nil then
            toolchain = build.toolchain
        elseif build.type then
            if config == 'host' and host_toolchain and
                self.name ~= host_toolchain
            then
                toolchain = host_toolchain
            elseif config == 'target' and target_toolchain and
                self.name ~= target_toolchain
            then
                toolchain = target_toolchain
            end
        end
    end
    return toolchain
end

function P:add_rule(rule, config)
    return self:add_target(Target:parse(rule, self.name, config))
end

function P:add_stage(name, config)
    return self:add_target(Target.from_args(self.name, name, config))
end

function P:add_target(target)
    -- always use the whole package as an instance instead of a config subset
    if self.config then
        self = self._pkg
    end
    local shared = { unpack = true, patch  = true }
    local name = target.stage
    if name == 'update' then name = 'unpack' end
    local config = not shared[name] and target.config
    local stages = self:get('stages', config)
    if not stages then
        stages = self:set('stages', {}, config)
    end
    local existing = stages[name]
    if existing then
        existing:add_inputs(target)
    else
        target.name = self.name -- for the case of adopting from other pkg
        existing = target
        stages[name] = target
        table.insert(stages, target)
    end
    return existing
end

function P:add_required_stage(config)
    local build, install, name = self:get('build', config), self:get('install', config)
    if install and install.type then
        name = 'install'
    elseif build and build.type then
        name = 'compile'
    else
        name = 'install'
    end
    return self:add_stage(name, config)
end

return P
