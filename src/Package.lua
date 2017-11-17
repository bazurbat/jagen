local System = require 'System'
local Target = require 'Target'
local Source = require 'Source'
local Log    = require 'Log'

local lua_package = package

local current_filename

local P = {}
P.__index = P

local packages = {}

local function try_load_module(modname)
    for path in string.gmatch(lua_package.path, '[^;]+') do
        local filename = string.gsub(path, '%?', modname)
        local file = io.open(filename, 'rb')
        if file then
            local module = assert(loadstring(assert(file:read('*a')), filename))
            file:close()
            return module(), filename
        end
    end
end

function P:__tostring()
    return string.format('%s__%s', self.name or '', self.config or '')
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

function P:new(rule)
    rule = P:parse(rule)
    setmetatable(rule, self)
    return rule
end

function P:has_config(name)
    return self.configs and self.configs[name]
end

function P:add_config(name)
    if not self.configs then
        self.configs = {}
    end
    if not self.configs[name] then
        self.configs[name] = {}
    end
end

function P:get(key, config)
    if config and self.configs and self.configs[config] then
        return self.configs[config][key]
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
end

function P:add_requires(stage, template)
    for _, item in ipairs(stage.requires or {}) do
        local req = P:parse(item)
        req.config = req.config or template.config
        if req.config ~= 'system' then
            table.insert(stage, { req.name, 'install', req.config })
            P.define_rule {
                name = req.name,
                template = template,
                { 'install' }
            }
        end
    end
end

function P:add_target(rule, config)
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

function P:add_patch_dependencies()
    local function patch_names(pkg)
        local i, n = 0, #pkg.patches
        return function()
            i = i + 1
            if i <= n then return pkg.patches[i][1] end
        end
    end

    local function get_provider(name)
        if name and name ~= 'none' then
            return packages[name] or P.define_irule { name }
        end
    end

    local function get_provided_filename(provider, name)
        return System.expand(System.mkpath(provider.source.dir,
            self.patches.dir or '', name..'.patch'))
    end

    local function find_in_path(name)
        return System.pread('*l', '%s find_patch "%s"', Jagen.cmd, name)
    end

    local function add_inputs(pkg, inputs)
        local stage = pkg.stages['unpack']
        stage.inputs = stage.inputs or {}
        table.iextend(stage.inputs, inputs)

        pkg.patches = pkg.patches or {}
        pkg.patches.required = extend(pkg.patches.required, inputs)
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

        pkg.patches = pkg.patches or {}
        pkg.patches.provided = sort(extend(pkg.patches.provided, outputs))
    end

    local provider = get_provider(self.patches.provider)
    local filenames = {}

    for name in patch_names(self) do
        local filename
        if provider then
            filename = get_provided_filename(provider, name)
        else
            filename = find_in_path(name)
        end
        if not filename then
            provider = get_provider('patches')
            filename = get_provided_filename(provider, name)
        end
        table.insert(filenames, filename)
    end

    add_inputs(self, filenames)
    if provider then
        add_outputs(provider, filenames)
    end
end

function P:add_ordering_dependencies()
    local prev, common 

    for curr in self:each() do
        if curr.stage == 'provide_patches' then
            local unpack = assert(self.stages['unpack'])
            curr.inputs = append(curr.inputs, unpack)
        else
            if prev then
                if common and curr.config ~= prev.config then
                    curr.inputs = append(curr.inputs, common)
                else
                    curr.inputs = append(curr.inputs, prev)
                end
            end

            prev = curr
            if not curr.config then
                common = curr
            end
        end
    end
end

function P:each()
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

function P:query(value, config)
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

function P.load_rules()
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
            P.define_irule { 'toolchain', 'host',
                requires = { 'gcc-native' }
            }
            break
        end
    end

    local target_toolchain = os.getenv('jagen_target_toolchain')
    if target_toolchain then
        P.define_irule {
            name = target_toolchain,
            config = 'target'
        }
        P.define_irule { 'toolchain', 'target',
            requires = { target_toolchain }
        }
    end

    -- As the add_patch_dependencies can insert new packages to the list
    -- the usage of the table.filter here is essential to avoid undefined
    -- behaviour during the traversal because we are relying on the fact
    -- that it returns a new list with the matching elements.
    table.for_each(table.filter(packages,
            function (pkg) return pkg.patches end),
        P.add_patch_dependencies)

    local source_exclude = os.getenv('jagen_source_exclude')
    for name in string.gmatch(source_exclude, '[^%s]+') do
        local pkg = packages[name]
        if pkg then
            pkg.source.exclude = true
        else
            Log.warning("the jagen_source_exclude list contains '%s' but no such package is defined", name)
        end
    end

    return packages
end

function P.define_rule(rule)
    rule = P:new(rule)

    local pkg = packages[rule.name]

    if not pkg then
        pkg = P:new { rule.name }
        pkg:add_target { 'unpack' }
        if pkg.name ~= 'patches' then
            pkg:add_target { 'patch' }
        end
        pkg.filenames = {}
        local module, filename = try_load_module('pkg/'..rule.name)
        if module then
            table.merge(pkg, P:new(module))
            table.insert(pkg.filenames, filename)
        end
        packages[rule.name] = pkg
        pkg.configs = pkg.configs or {}
    end

    if rule.template then
        rule = table.merge(copy(rule.template), rule)
    end

    do
        local prev = pkg.filenames[#pkg.filenames]
        if not rule.implicit and (not prev or not string.find(prev, current_filename, 1, true)) then
            table.insert(pkg.filenames, current_filename)
        end
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

    local template = rule.template or rule.pass_template or this.template or {}
    template.config = config

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

    if not pkg.source or not getmetatable(pkg.source) then
        pkg.source = Source:create(pkg.source, pkg.name)
        if pkg.patches and pkg.source:is_scm() then
            pkg.source.ignore_dirty = true
        end
    end

    if pkg.source and pkg.source.type == 'repo' then
        pkg:add_target { 'unpack',
            { 'repo', 'install', 'host' }
        }
        P.define_irule { 'repo', 'host' }
    end

    if config then
        local build = this.build or pkg.build
        local install = this.install or pkg.install

        if build then
            if build.in_source and pkg.source:is_scm() then
                pkg.source.ignore_dirty = true
            end

            if build.type == 'GNU' then
                if build.generate or build.autoreconf then
                    pkg:add_target { 'autoreconf',
                        { 'libtool', 'install', 'host' }
                    }
                    P.define_irule { 'libtool', 'host' }
                end
            end

            if pkg.name == 'toolchain' or build.requires_toolchain == false then
                pkg:add_target({ 'configure' }, config)
            else
                pkg:add_target({ 'configure',
                        { 'toolchain', 'install', config }
                    }, config)
                P.define_irule { 'toolchain', config }
            end

            if build.type == 'linux_module' or build.kernel_modules == true or
                    install and install.modules then

                P.define_irule { 'kernel', config }

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
    if config then
        for _, stage in ipairs(pkg) do
            pkg:add_requires(stage, template)
            pkg:add_target(stage, config)
        end
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

function P.define_irule(rule)
    rule = rule or {}
    rule.template = rule.template or {}
    rule.template.implicit = true
    return P.define_rule(rule)
end

function define_package_alias(name, value)
    Jagen.package_aliases[name] = value
end

function package(rule)
    return P.define_rule(rule)
end

return P
