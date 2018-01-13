local System = require 'System'
local Target = require 'Target'
local Source = require 'Source'
local Log    = require 'Log'

local P = {}
P.__index = P

local lua_package = package
local packages = {}

local current_context
local context_stack = {}

local Context = {}

function Context:new(o)
    o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Context:__unm()
    local this = self
    local s = ''
    while this do
        for k, v in pairs(this) do
            if k ~= 'parent' then
                s = string.format('%s:%s=%s', s, k, tostring(v))
            end
        end
        this = this.parent
    end
    return s
end

local function push_context(new)
    new = Context:new(new)
    new.parent = current_context
    table.insert(context_stack, new)
    current_context = new
    return new
end

local function pop_context()
    local o = assert(table.remove(context_stack))
    current_context = context_stack[#context_stack]
    return o
end

local function find_module(modname)
    for path in string.gmatch(lua_package.path, '[^;]+') do
        local filename = string.gsub(path, '%?', modname)
        local file = io.open(filename, 'rb')
        if file then
            local module = assert(loadstring(assert(file:read('*a')), filename))
            file:close()
            return module, filename
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
                config = req.config,
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
            return packages[name] or P.define_rule { name }
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
        -- Adding patch files to arguments modifies the command line which is
        -- needed for Ninja to notice the changes in the list itself and rerun
        -- the command.
        stage.arg = inputs

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
        -- Adding patch files to arguments modifies the command line which is
        -- needed for Ninja to notice the changes in the list itself and rerun
        -- the command which then checks if the patches were indeed provided.
        stage.arg = stage.outputs

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

function P:each_config()
    return function (t, i)
        if self.configs then
            return next(self.configs, i)
        else
            return nil
        end
    end
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

function P:check_insource_build()
    local config_count = table.count(self.configs)
    local in_source, generate
    if self.build then
        in_source = self.build.in_source
        generate = self.build.generate
    end
    for _, config in pairs(self.configs or {}) do
        if config.build then
            in_source = in_source or config.build.in_source
            generate = generate or config.build.generate
        end
    end
    if config_count > 1 then
        if in_source then
            Log.warning("the package '%s' requires in source build but have multiple configs defined, this will likely produce incorrect results", self.name)
        end
    end
end

function P.load_rules()
    local def_loader = lua_package.loaders[2]
    lua_package.loaders[2] = find_module

    packages = {}

    local dirs = string.split2(System.pread('*a', '"%s" get_path', Jagen.cmd), '\t')

    for i = 1, #dirs do
        local filename = System.mkpath(dirs[i], 'rules.lua')
        local file = io.open(filename, 'rb')
        if file then
            assert(loadstring(file:read('*a'), filename))()
            file:close()
        end
    end

    push_context({ implicit = true })

    -- As the add_patch_dependencies can insert new packages to the list
    -- the usage of the table.filter here is essential to avoid undefined
    -- behaviour during the traversal because we are relying on the fact
    -- that it returns a new list with the matching elements.
    table.for_each(table.filter(packages,
            function (pkg) return pkg.patches end),
        P.add_patch_dependencies)

    local host_toolchain = 'gcc-native'
    local target_toolchain = os.getenv('jagen_target_toolchain')

    for name, pkg in pairs(table.copy(packages)) do
        for config, _ in pkg:each_config() do
            local build = pkg:get('build', config)
            if build and build.type then
                local toolchain = build.toolchain
                if toolchain ~= false then
                    if config == 'host' and host_toolchain and
                        name ~= host_toolchain then
                        toolchain = host_toolchain
                    elseif config == 'target' and target_toolchain and
                        name ~= target_toolchain then
                        toolchain = target_toolchain
                    end
                    if toolchain then
                        build.toolchain = toolchain
                        push_context(table.merge(copy(current_context), {
                                    name = name,
                                    config = config
                            }))
                        local stage = { 'configure', requires = { toolchain } }
                        pkg:add_requires(stage, { config = config })
                        pkg:add_target(stage, config)
                        pop_context()
                    end
                end
            end
        end
    end

    local source_exclude = os.getenv('jagen_source_exclude')
    for name in string.gmatch(source_exclude, '[^%s]+') do
        local pkg = packages[name]
        if pkg then
            pkg.source.exclude = true
        else
            Log.warning("the jagen_source_exclude list contains '%s' but no such package is defined", name)
        end
    end

    for name, pkg in pairs(packages) do
        pkg:check_insource_build()
    end

    lua_package.loaders[2] = def_loader

    return packages
end

function P.define_rule(rule, context)
    rule = P:new(rule)

    local pkg = packages[rule.name]

    if not pkg then
        pkg = P:new { rule.name }
        pkg.contexts = {}
        pkg:add_target { 'unpack' }
        if pkg.name ~= 'patches' then
            pkg:add_target { 'patch' }
        end
        local module, filename = find_module('pkg/'..rule.name)
        if module then
            table.merge(pkg, P:new(assert(module())))
            append(pkg.contexts, Context:new { filename = filename })
        end
        packages[rule.name] = pkg
        pkg.configs = pkg.configs or {}
    end

    if rule.template then
        rule = table.merge(copy(rule.template), rule)
    end

    if context then
        context.name = rule.name
        context.config = rule.config
        push_context(context)
    end

    append(pkg.contexts, assert(current_context))

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

    local template = {}
    if rule.template ~= false then
        template = rule.template or rule.pass_template or this.template or {}
    end
    template.config = config

    rule.template, rule.pass_template = nil, nil
    table.merge(this, rule)
    this.template = template

    if this ~= pkg then
        if pkg.build then
            if not this.build then this.build = {} end
            if not getmetatable(this.build) then
                setmetatable(this.build, { __index = pkg.build })
            end
        end
        if pkg.install then
            if not this.install then this.install = {} end
            if not getmetatable(this.install) then
                setmetatable(this.install, { __index = pkg.install })
            end
        end
    end

    if not pkg.source or not getmetatable(pkg.source) then
        pkg.source = Source:create(pkg.source, pkg.name)
        if pkg.patches and pkg.source:is_scm() then
            pkg.source.ignore_dirty = true
        end
    end

    push_context({
            name = pkg.name,
            config = config,
            implicit = true
        })

    if pkg.source and pkg.source.type == 'repo' then
        pkg:add_target { 'unpack',
            { 'repo', 'install', 'host' }
        }
        P.define_rule { 'repo', 'host' }
    end

    if config then
        local build = this.build or pkg.build
        local install = this.install or pkg.install

        if build then
            if build.in_source then
                if not build.dir then
                    build.dir = '$pkg_source_dir'
                end
                if pkg.source:is_scm() then
                    pkg.source.ignore_dirty = true
                end
            end

            if build.type == 'GNU' then
                if build.generate or build.autoreconf then
                    pkg:add_target { 'autoreconf',
                        { 'libtool', 'install', 'host' }
                    }
                    P.define_rule { 'libtool', 'host' }
                end
            end

            if build.type == 'linux_module' or build.kernel_modules == true or
                    install and install.modules then

                P.define_rule { 'kernel', config }

                pkg:add_target({ 'configure',
                        { 'kernel', 'configure', config }
                    }, config)
                pkg:add_target({ 'compile',
                        { 'kernel', 'compile', config }
                    }, config)
                pkg:add_target({ 'install',
                        { 'kernel', 'install', config }
                    }, config)
            elseif build.type then
                pkg:add_target({ 'configure' }, config)
                pkg:add_target({ 'compile' }, config)
            end

            if config == 'target' and build.target_requires_host then
                rule.requires = append(rule.requires or {}, { pkg.name, 'host' })
            end

            if build.target_arch or build.target_system then
                local export = {
                    arch   = build.target_arch,
                    system = build.target_system,
                    dir    = build.dir
                }
                if next(export) then
                    pkg.export = pkg.export or {}
                    table.merge(pkg.export, export)
                end
            end
        end

        if install then
            if install.type and (install.type ~= 'none' or install.type ~= false) then
                pkg:add_target({ 'install' }, config)
            end
        elseif build and build.type then
            pkg:add_target({ 'install' }, config)
        end
    end

    pop_context()

    local pkg_context
    if not context then
        pkg_context = push_context({
                name = pkg.name,
                config = config,
                implicit = current_context.implicit 
            })
    end

    -- add global stages to every config
    if config then
        for _, stage in ipairs(pkg) do
            pkg:add_requires(stage, template)
            pkg:add_target(stage, config)
        end
    end

    -- always evaluate shared requires in config-specific context
    local requires = copy(pkg.requires or {})
    for_each(rule.requires, append_uniq, requires)
    if config and #requires > 0 then
        local name = pkg.build and 'configure' or 'install'
        local stage = { name, requires = requires }
        pkg:add_requires(stage, template)
        pkg:add_target(stage, config)
    end

    -- stages collected from this rule should go last to maintain ordering
    for _, stage in ipairs(stages) do
        pkg:add_requires(stage, template)
        pkg:add_target(stage, config)
    end

    if pkg_context then pop_context() end

    if context then pop_context() end

    return pkg
end

function define_package_alias(name, value)
    Jagen.package_aliases[name] = value
end

function package(rule)
    local context, level, info = {}, 2
    repeat
        info = debug.getinfo(level, 'Sl')
        level = level+1
    until not info or info.what == 'main'
    if info then
        context.filename = info.source
        context.line = info.currentline
    end
    return P.define_rule(rule, context)
end

return P
