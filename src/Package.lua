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

    if type(rule.source) == 'string' then
        if string.match(rule.source, '^https?://') then
            rule.source = { type = 'curl', location = rule.source }
        else
            rule.source = { type = 'dist', location = rule.source }
        end
    end

    if type(rule.use) == 'string' then
        rule.use = { rule.use }
    end

    if rule.build ~= nil and type(rule.build) ~= 'table' then
        rule.build = { type = rule.build }
    end

    if rule.install ~= nil and type(rule.install) ~= 'table' then
        rule.install = { type = rule.install }
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
end

function P:add_requires(stage, config, template)
    local added = {}
    for _, item in ipairs(stage.requires or {}) do
        local req = P:parse(item)
        req.config = req.config or config or template and template.config
        if req.config ~= 'system' then
            table.insert(stage, { req.name, 'install', req.config })
            table.insert(added, P.define_rule {
                    name = req.name,
                    config = req.config,
                    template = template
                })
        end
    end
    return added
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

function P:export_build_env()
    local function export_build_env(this, config)
        local build = self:get('build', config)
        if build then
            if build.cflags and build.cxxflags == nil then
                build.cxxflags = build.cflags
            end
            local export = self:get('export', config)
            if export then
                for key in each { 'arch', 'system', 'cpu',
                                  'cflags', 'cxxflags', 'ldflags'
                                } do
                    if export[key] == nil then
                        export[key] = build[key]
                    end
                end
            end
        end
    end
    export_build_env(self, config)
    for config, this in self:each_config() do
        export_build_env(this, config)
    end
end

function P:export_dirs()
    local export = self.export
    local source = self.source
    if source then
        export.source = export.source or {}
        export.source.dir = source.dir
    end
end

function P:add_export_stages()
    if self.export and not self.stages['export'] then
        self:add_target { 'export' }
    end
    for config, this in self:each_config() do
        if not this.stages then this.stages = {} end
        if this.export and not this.stages['export'] then
            local target = Target:new(self.name, 'export', config)
            this.stages['export'] = target
            table.insert(this.stages, 1, target)
        end
    end
end

function P:add_ordering_dependencies()
    local prev, common 

    for curr in self:each() do
        if curr.stage == 'provide_patches' then
            local unpack = assert(self.stages['unpack'])
            curr.inputs = append(curr.inputs, unpack)
        elseif curr.stage == 'export' then
            curr.inputs = append(curr.inputs, assert(common))
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
        return assert(System.pread('*l', 'jagen-stage -q %q %q %q',
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

function P:check_build_configs()
    if self.build and self.build.type and table.count(self.configs) == 0 then
        Log.warning("the package '%s' requires a build but has no configs defined",
            self.name)
    end
end

function P:check_build_insource()
    local count, had_warnings, build = table.count(self.configs)
    for config, this in self:each_config() do
        build = this.build
        if build and build.in_source and count > 1 then
            Log.warning("the package '%s' requires an in source build but "..
                "has multiple configs defined -- this is not supported",
                self.name)
            had_warnings = true
            break
        end
    end
    return had_warnings
end

function P:check_build_toolchain()
    local had_warnings, build
    for config, this in self:each_config() do
        build = this.build
        if build and build.type and not build.toolchain then
            Log.warning("the package '%s' requires '%s' build for "..
                "config '%s' but does not have a toolchain set", self.name,
                build.type, config)
            had_warnings = true
        end
    end
    return had_warnings
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

    function add_toolchains(pkglist)
        local added = {}
        for _, pkg in pairs(pkglist) do
            for config, _ in pkg:each_config() do
                local build = pkg:get('build', config)
                if build and (build.type or build.toolchain) then
                    local toolchain = build.toolchain
                    if toolchain == nil then
                        if config == 'host' and host_toolchain and
                            pkg.name ~= host_toolchain then
                            toolchain = host_toolchain
                        elseif config == 'target' and target_toolchain and
                            pkg.name ~= target_toolchain then
                            toolchain = target_toolchain
                        end
                    end
                    if toolchain then
                        build.toolchain = toolchain
                        push_context(table.merge(copy(current_context), {
                                    name = pkg.name,
                                    config = config
                            }))
                        local stage = { build.type and 'configure' or 'install',
                            requires = { toolchain }
                        }
                        extend(added, pkg:add_requires(stage, config))
                        pkg:add_target(stage, config)
                        pop_context()
                    end
                end
            end
        end
        return added
    end

    do
        local packages = table.copy(packages)
        repeat
            packages = add_toolchains(packages)
        until #packages == 0
    end

    -- another pass, with the toolchains this time
    for _, pkg in pairs(packages) do
        pkg:export_dirs()
        pkg:export_build_env()
        pkg:add_export_stages()
    end

    local source_exclude = os.getenv('jagen_source_exclude')
    local function is_scm(pkg)
        return pkg.source and pkg.source:is_scm()
    end
    for item in string.gmatch(source_exclude, '%S+') do
        local invert = item:sub(1, 1) == '!'
        local shpat = invert and item:sub(2) or item
        if #shpat < 1 then
            Log.warning("invalid pattern '%s' in jagen_source_exclude list", item)
        end
        local luapat, match, matched = shpat:convert_pattern()
        for name, pkg in iter(packages, filter(is_scm)) do
            match = name:match(luapat)
            if (match and not invert) or (invert and not match) then
                matched = true
                if pkg.source then
                    pkg.source.exclude = true
                end
            end
        end
        if not matched then
            Log.warning("could not find SCM package matching '%s' from jagen_source_exclude list", item)
        end
    end

    local had_warnings = false
    for name, pkg in pairs(packages) do
        had_warnings = pkg:check_build_configs() or had_warnings
        had_warnings = pkg:check_build_insource() or had_warnings
        had_warnings = pkg:check_build_toolchain() or had_warnings
    end

    if had_warnings then
        error('failed to load rules due to warnings')
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
        pkg.export = pkg.export or {}
    end

    if rule.template then
        rule = table.merge(copy(rule.template), rule)
    end

    if context then
        context.name = rule.name
        context.config = rule.config
        push_context(context)
    end

    if current_context then
        append(pkg.contexts, current_context)
    end

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

    local template
    if rule.template ~= false then
        template = rule.template or rule.pass_template or this.template
    end

    rule.template, rule.pass_template = nil, nil
    table.merge(this, rule)
    this.template = template

    -- do not collect stage rules in the config
    table.iclean(this)

    if this ~= pkg then
        if pkg.build then
            if not this.build then this.build = {} end
            if not getmetatable(this.build) then
                setmetatable(this.build, { __index = pkg.build })
            end
            if not pkg.install then pkg.install = {} end
        end
        if pkg.install or this.build then
            if not this.install then this.install = {} end
            if not getmetatable(this.install) then
                setmetatable(this.install, { __index = pkg.install })
            end
        end
        if not this.export then this.export = {} end
        if not getmetatable(this.export) then
            setmetatable(this.export, { __index = pkg.export })
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

    if pkg.source and pkg.source.dir then
        local export = pkg.export
        if export.dir == nil then
            export.dir = pkg.source.dir
        end
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
        end

        if install then
            if install.type == nil and build and build.type then
                install.type = build.type
            end
            if install.type and install.type ~= false then
                pkg:add_target({ 'install' }, config)
            end
        elseif build and build.type then
            pkg:add_target({ 'install' }, config)
        end
    end

    pop_context()

    if not context then
        context = push_context({
                name = pkg.name,
                config = config,
                implicit = current_context and current_context.implicit 
            })
    end

    local stages = extend({}, pkg)

    local requires = extend(extend({}, pkg.requires), rule.requires)
    if config and #requires > 0 then
        local name = this.build and this.build.type and 'configure' or 'install'
        append(stages, { name, requires = requires })
    end

    extend(stages, rule)

    for stage in each(stages) do
        pkg:add_requires(stage, config, template)
        pkg:add_target(stage, config)
    end

    if context then pop_context() end

    for use in each(pkg.use or {}) do
        local used = packages[use] or P.define_rule { use }
        if used.export then
            pkg:add_target { 'unpack', { use, 'export' } }
        end
        for config, this in pkg:each_config() do
            if next(used:get('export', config) or {}) then
                for target in each(this.stages) do
                    if target.stage ~= 'export' then
                        target:add_inputs(Target:parse({ target.name,
                                    { use, 'export', config }
                            }, name, config))
                        break
                    end
                end
            end
        end
    end

    return pkg
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
