local System = require 'System'
local Target = require 'Target'
local Source = require 'Source'
local Log    = require 'Log'

local current_filename

local Package = {}
Package.__index = Package

local packages = {}

local function try_load_module(modname)
    for path in string.gmatch(package.path, '[^;]+') do
        local filename = string.gsub(path, '%?', modname)
        local file = io.open(filename, 'rb')
        if file then
            local module = assert(loadstring(assert(file:read('*a')), filename))
            file:close()
            return module()
        end
    end
end

function Package:__tostring()
    return string.format('%s__%s', self.name or '', self.config or '')
end

function Package:parse(rule)
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

    rule.name = Jagen.package_aliases[rule.name] or rule.name

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

function Package:add_requires(stage, template)
    for _, item in ipairs(stage.requires or {}) do
        local req = Package:parse(item)
        req.config = req.config or template.config
        if req.config ~= 'system' then
            table.insert(stage, { req.name, 'install', req.config })
            define_rule {
                name = req.name,
                template = template,
                { 'install' }
            }
        end
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

function Package.add_patch_dependencies()
    local function having_patches(pkg)
        return pkg.patches
    end

    local function patches_provider(pkg)
        local name = pkg.patches.provider or 'patches'
        return packages[name] or define_rule { name }
    end

    local function add_inputs(pkg, inputs)
        local stage = pkg.stages['unpack']
        stage.inputs = stage.inputs or {}
        table.iextend(stage.inputs, inputs)
    end

    local function add_outputs(pkg, outputs)
        local name = 'provide_patches'
        local stage = pkg.stages[name]
        if not stage then
            pkg:add_target { name }
            stage = assert(pkg.stages[name])
        end
        stage.outputs = stage.outputs or {}
        table.iextend(stage.outputs, outputs)
    end

    local function get_name(item)
        return item[1]
    end

    local function map_names(pkg, func)
        return table.imap(pkg.patches, compose(func, get_name))
    end

    local function add_dependencies(pkg)
        local provider = patches_provider(pkg)

        local function get_source_path(name)
            local source_dir = provider.source.dir
            local filename = name..'.patch'
            local path
            if pkg.patches.dir then
                path = System.mkpath(source_dir, pkg.patches.dir, filename)
            else
                path = System.mkpath(source_dir, filename)
            end
            return System.expand(path)
        end

        add_inputs(pkg, map_names(pkg, get_source_path))
        add_outputs(provider, map_names(pkg, get_source_path))
    end

    table.for_each(table.filter(packages, having_patches), add_dependencies)
end

function Package:each()
    return coroutine.wrap(function ()
            for _, target in ipairs(self.stages) do
                coroutine.yield(target)
            end
            if self.configs then
                -- Having consistent order everywhere helps with debugging and
                -- diffing of output files. Actually sort algorithm in Lua 5.1
                -- is not stable but this is not an issue for now.
                local names = {}
                for name, _ in pairs(self.configs) do
                    table.insert(names, name)
                end
                table.sort(names)
                for _, name in ipairs(names) do
                    local config = self.configs[name]
                    for _, target in ipairs(config.stages or {}) do
                        coroutine.yield(target)
                    end
                end
            end
        end)
end

function Package:query(value, config)
    local result = {}

    local function run_query(config)
        return assert(System.pread('*l', 'jagen-pkg -q %q %q %q',
            assert(value), assert(self.name), config or ''))
    end

    if config then
        assert(self:has_config(config),
            "the package '"..self.name.."' does not have the config '"..config.."'")
        result[config] = run_query(config)
    elseif next(self.configs) then
        for config, _ in pairs(self.configs) do
            result[config] = run_query(config)
        end
    else
        result['__'] = run_query()
    end

    return result
end

function Package.load_rules(full)
    local dirs = string.split2(os.getenv('jagen_path'), '\t')

    packages = {}

    for i = #dirs, 1, -1 do
        local filename = System.mkpath(dirs[i], 'rules.lua')
        local file = io.open(filename, 'rb')
        if file then
            current_filename = filename
            assert(loadstring(file:read('*a'), filename))()
            file:close()
        end
    end

    for _, pkg in pairs(packages) do
        if pkg.name == 'toolchain' and pkg:has_config('host') then
            define_rule { 'toolchain', 'host',
                requires = { 'gcc-native' }
            }
            break
        end
    end

    local target_toolchain = os.getenv('jagen_target_toolchain')
    if target_toolchain then
        define_rule {
            name = target_toolchain,
            config = 'target'
        }
        define_rule { 'toolchain', 'target',
            requires = { target_toolchain }
        }
    end

    Package.add_patch_dependencies()

    for _, pkg in pairs(packages) do
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
        pkg:add_target { 'unpack' }
        if pkg.name ~= 'patches' then
            pkg:add_target { 'patch' }
        end
        local module = try_load_module('pkg/'..rule.name)
        if module then
            table.merge(pkg, Package:new(module))
        end
        packages[rule.name] = pkg
        pkg.configs = pkg.configs or {}

        pkg.source = Source:create(pkg.source, pkg.name)
        if pkg.patches or (pkg.build and pkg.build.in_source) then
            pkg.source.ignore_dirty = true
        end
    end

    if rule.template then
        rule = table.merge(copy(rule.template), rule)
    end

    local stages = table.imove({}, rule)

    local config = rule.config
    local this

    if config then
        this = pkg.configs[config]
        if not this then
            this = {}
            pkg.configs[config] = this
        end
    else
        this = pkg
    end

    for _, key in ipairs({ 'source', 'patches' }) do
        if rule[key] then
            pkg[key] = table.merge(pkg[key] or {}, rule[key])
            rule[key] = nil
        end
    end

    local template = rule.template or rule.pass_template or this.template

    if not template then
        template = { config = config }
    end

    rule.template, rule.pass_template = nil, nil
    table.merge(this, rule)
    this.template = template

    if this ~= pkg then
        if this.build and pkg.build and not getmetatable(this.build) then
            setmetatable(this.build, { __index = pkg.build })
        end
        if this.install and pkg.install and not getmetatable(this.install) then
            setmetatable(this.install, { __index = pkg.install })
        end
    end

    if pkg.source and pkg.source.type == 'repo' then
        pkg:add_target { 'unpack',
            { 'repo', 'install', 'host' }
        }
        define_rule { 'repo', 'host' }
    end

    if config then
        local build = this.build or pkg.build
        local install = this.install or pkg.install

        if build then
            if pkg.name ~= 'toolchain' then
                define_rule { 'toolchain', config }
            end

            if build.type == 'GNU' then
                if build.generate or build.autoreconf then
                    pkg:add_target { 'autoreconf',
                        { 'libtool', 'install', 'host' }
                    }
                    define_rule { 'libtool', 'host' }
                end
            end

            if pkg.name == 'toolchain' or build.toolchain == false then
                pkg:add_target({ 'configure' }, config)
            else
                pkg:add_target({ 'configure',
                        { 'toolchain', 'install', config }
                    }, config)
            end

            if build.type == 'linux_module' or build.kernel_modules == true or
                    install and install.modules then

                define_rule { 'kernel', config }

                pkg:add_target({ 'configure',
                        { 'kernel', 'configure', config }
                    }, config)
                pkg:add_target({ 'compile',
                        { 'kernel', 'compile', config }
                    }, config)
                pkg:add_target({ 'install',
                        { 'kernel', 'install', config }
                    }, config)
            else
                pkg:add_target({ 'compile' }, config)
                pkg:add_target({ 'install' }, config)
            end
        end
    end

    -- add global stages to every config
    for _, stage in ipairs(pkg) do
        pkg:add_requires(stage, template)
        pkg:add_target(stage, config)
    end

    -- always evaluate shared requires in config-specific context
    local requires = extend(extend({}, pkg.requires), rule.requires)
    if config and #requires > 0 then
        local configure = { 'configure', requires = requires }
        pkg:add_requires(configure, template)
        pkg:add_target(configure, config)
    end

    -- stages collected from this rule should go last to maintain ordering
    for _, stage in ipairs(stages) do
        pkg:add_requires(stage, template)
        pkg:add_target(stage, config)
    end

    return pkg
end

function define_package_alias(name, value)
    Jagen.package_aliases[name] = value
end

return Package
