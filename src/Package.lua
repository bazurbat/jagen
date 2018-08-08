local System = require 'System'
local Target = require 'Target'
local Source = require 'Source'
local Log    = require 'Log'
local Command = require 'Command'

local P = {}
P.__index = P

local had_errors = false
local had_warnings = false
local function print_error(...)
    Log.error(...)
    had_errors = true
end
local function print_warning(...)
    Log.warning(...)
    had_warnings = true
end

local lua_package = package
local packages = {}
local used_packages = {}
local F = {
    used_requires = {}
}

local current_context
local context_stack = {}

local Context = {}

function Context:new(o)
    o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Context:__tostring()
    local insert, concat = table.insert, table.concat
    local lines = {}
    local function append(...)
        for i = 1, select('#', ...) do
            insert(lines, (select(i, ...)))
        end
    end
    if self.name or self.config then
        append(concat({ self.name, self.config }, ':'))
    end
    if self.filename then
        if #lines > 0 then append(' ') end
        if self.name or self.config then append('(') end
        local filename, removed = self.filename:remove_prefix(System.dirname(Jagen.project_dir))
        if removed then append('...') end append(filename)
        if self.line then append(':', self.line) end
        if self.name or self.config then append(')') end
    end
    if self.implicit and #lines > 0 then
        append(' *')
    end
    return table.concat(lines)
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

function P:__tostring(sep)
    local c = {}
    if self.name then table.insert(c, self.name) end
    if self.config then table.insert(c, self.config) end
    return table.concat(c, sep or ':')
end

function P:format_contexts(start_col, start_1col)
    start_col = start_col or 0
    start_1col = start_1col or start_col
    local lines = {}
    for i = 1, #self.contexts do
        local context, level = self.contexts[i], 0
        while context do
            local contextstr = tostring(context)
            if #contextstr > 0 then
                table.insert(lines, string.rep('  ', level)..contextstr)
            end
            context = context.parent
            level = level + 1
        end
    end
    local res = table.concat(lines, '\n'..string.rep(' ', start_col))
    if start_1col then
        return string.rep(' ', start_1col)..res
    else
        return res
    end
end

function P:format_last_context()
    return tostring(self.contexts[#self.contexts]) or '<unknown>'
end

function P:format_at()
    return string.format('\n----\n at: %s\n', self:format_contexts(5, 0))
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

    parse_section('build')
    parse_section('install')

    if rule.build and rule.build.clean ~= nil then
        local clean = rule.build.clean
        if type(clean) ~= 'table' then
            rule.build.clean = { clean }
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
    return self.configs[name]
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

function P:last(config)
    local stages = self:get('stages', config)
    if stages then return stages[#stages] end
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
                        return a.config < b.config
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
                coroutine.yield('-', self)
            end
            if self.configs then
                for k, v in pairs(self.configs) do
                    coroutine.yield(k, v)
                end
            end
        end)
end

function P:each_config2(with_shared)
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

function P:gettoolchain(config)
    local host_toolchain = 'system-native'
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

function P:add_stage(name, config)
    local stages = self:get('stages', config)
    if not stages then
        stages = self:set('stages', {}, config)
    end
    local target = stages[name]
    if not target then
        target = Target.from_args(self.name, name, config)
        stages[name] = target
        table.insert(stages, target)
    end
    return target
end

function P:add_rule(rule, config)
    local shared = { unpack = true, patch  = true, autoreconf = true }
    local target = Target:parse(rule, self.name, config)
    local name   = target.stage
    local config = not shared[name] and target.config
    return self:add_stage(name, config):add_inputs(target)
end

function P:add_stages(stages, config, template)
    for stage in each(stages) do
        local target = self:add_rule(stage, config)
        for spec in each(stage.requires) do
            local template = stage.requires.template or template
            self:add_last_stage(target.stage, spec, { config = config, template = template })
        end
    end
end

function P:add_require(spec, context)
    local key = string.format('%s^%s^%s^%s', self.name, spec,
        tostring(context.config), tostring(context.template))
    F.used_requires[key] = { self, spec, context }
end

function P:define_require(spec, context)
    local config, template = context.config, context.template
    local build, install, stage = self:get('build', config), self:get('install', config)
    if build and build.type then
        stage = 'configure'
    elseif install and install.type then
        stage = 'install'
    else
        stage = self:last(config).stage
    end
    return self:add_last_stage(stage, spec, context)
end

function P:add_last_stage(stage, spec, context)
    local config, template = context.config, context.template
    local use, use_config = self:define_use(spec, context)
    if use then
        self:add_stage(stage, config):append(use:last(use_config))
    end
    return use
end

function P:add_patch_dependencies()
    local new_packages = {}

    local function patch_names(pkg)
        local i, n = 0, #pkg.patches
        return function()
            i = i + 1
            if i <= n then return pkg.patches[i][1] end
        end
    end

    local function get_provider(name)
        if name and name ~= 'none' then
            local pkg = packages[name]
            if not pkg then
                pkg = P.define_package { name }
                new_packages[pkg.name] = pkg
                pkg.source:derive_properties(name)
            end
            return pkg
        end
    end

    local function get_provided_filename(provider, name)
        return System.expand(System.mkpath(assert(provider.source.dir),
            self.patches.dir or '', name..'.patch'))
    end

    local function find_in_path(name)
        return System.pread('*l', '%s find_patch "%s"', Jagen.cmd, name)
    end

    local function add_inputs(pkg, inputs)
        local stage = pkg.stages['unpack']
        pkg.patches = pkg.patches or {}

        -- Adding patch files to arguments modifies the command line which is
        -- needed for Ninja to notice the changes in the list itself and rerun
        -- the command.
        for input in each(inputs) do
            stage.inputs = append_uniq(input, stage.inputs)
            stage.arg = append_uniq(input, stage.arg)
            pkg.patches.required = append_uniq(input, pkg.patches.required)
        end

        table.sort(stage.arg or {})
        table.sort(pkg.patches.required or {})
    end

    local function add_outputs(pkg, outputs)
        local name = 'provide_patches'
        local stage = pkg.stages[name]
        if not stage then
            pkg:add_stage(name)
            stage = assert(pkg.stages[name])
        end

        pkg.patches = pkg.patches or {}

        -- Adding patch files to arguments modifies the command line which is
        -- needed for Ninja to notice the changes in the list itself and rerun
        -- the command which then checks if the patches were indeed provided.
        for output in each(outputs) do
            stage.outputs = append_uniq(output, stage.outputs)
            stage.arg = append_uniq(output, stage.arg)
            pkg.patches.provided = append_uniq(output, pkg.patches.provided)
        end

        table.sort(stage.arg or {})
        table.sort(pkg.patches.provided or {})
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
        append_uniq(filename, filenames)
    end

    add_inputs(self, filenames)
    if provider then
        add_outputs(provider, filenames)
    end

    return new_packages
end

function P:export_build_env()
    local keys = { 'cc', 'cxx', 'arch', 'system', 'cpu',
                   'cflags', 'cxxflags', 'ldflags' }
    for this, config in self:each_config2(true) do
        local build = this.build
        if build.cxxflags == nil and build.cflags ~= nil then
            build.cxxflags = build.cflags
        end
    end
    for this, config in self:each_config2() do
        local build, export = this.build, this.export
        for key in each(keys) do
            if rawget(export, key) == nil then export[key] = build[key] end
        end
    end
end

function P:add_toolchain_requires()
    for this, config in self:each_config2() do
        local build = this.build
        if build then
            local toolchain = build.toolchain
            if toolchain then
                self:add_require(toolchain, { config = config })
            end
        end
    end
end

function P:add_toolchain_uses()
    for this, config in self:each_config2(true) do
        local build = this.build
        if build then
            local toolchain = rawget(build, 'toolchain')
            if toolchain then
                -- self:add_require(toolchain, { config = config })
                this.uses = append_uniq(toolchain, this.uses)
            end
        end
    end
end

function P:export_dirs()
    local function export_build_dir(this, config)
        local export = this.export
        local build = this.build
        if build and export then
            export.build = rawget(export, 'build') or {}
            local dir = rawget(build, 'dir')
            if export.build.dir == nil then
                export.build.dir = dir
            end
        end
    end
    export_build_dir(self)
    for config, this in self:each_config() do
        export_build_dir(this, config)
    end
    local export, source = self.export, self.source
    if source then
        export.source = export.source or {}
        if export.source.dir == nil then
            export.source.dir = source.dir
        end
        if export.dir == nil then
            export.dir = source.dir
        end
    end
end

-- Defines an empty rule with 'host' config if the package definition was found
-- with some build.type which is not required by any other package (orphan).
function P.add_default_host_build_config(used_requires)
    local new_packages, required_names = {}, {}
    for _, item in pairs(used_requires) do
        local target = Target.from_use(item[2])
        required_names[target.name] = true
    end
    -- define_package modifies the package list, need to copy it to avoid
    -- undefined behaviour on traversal
    for _, pkg in pairs(table.copy(packages)) do
        local build = pkg.build
        if build and build.type and not next(pkg.configs) and
                not required_names[pkg.name] then
            local new_pkg
            if build.type == 'gradle-android' then
                new_pkg = P.define_package { pkg.name, 'target' }
            else
                new_pkg = P.define_package { pkg.name, 'host' }
            end
            new_packages[new_pkg.name] = new_pkg
        end
    end
    return new_packages
end

function P:add_ordering_dependencies()
    local prev, common 

    for curr in self:each() do
        if curr.stage == 'provide_patches' then
            local unpack = assert(self.stages['unpack'])
            curr.inputs = append(curr.inputs, unpack)
        elseif curr.stage == 'export' and curr.config then
            curr:append(assert(common))
            curr:append(Target.from_args(common.name, 'export'))
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

function P:check_build_configs()
    if self.build and self.build.type and table.count(self.configs) == 0 then
        print_error("the package '%s' requires a build but has no configs defined",
            self.name)
    end
end

function P:check_build_insource()
    local count, build = table.count(self.configs)
    for config, this in self:each_config() do
        build = this.build
        if build and build.in_source and build.in_source ~= 'multi' and count > 1 then
            print_warning("the package '%s' builds in source but has multiple configs defined, "..
                "please make sure that its build system supports this and set in_source='multi' "..
                "property to remove this warning",
                self.name)
            break
        end
    end
end

function P:check_build_toolchain()
    local build
    for config, this in self:each_config() do
        build = this.build
        if build and build.type and build.toolchain == nil then
            print_error("the package '%s' requires '%s' build for "..
                "config '%s' but does not have a toolchain set", self.name,
                build.type, config)
        end
    end
end

function P:check_usages()
    local implicit = true
    for context in each(self.contexts) do
        if not context.implicit then
            implicit = false
            break
        end
    end
    if not implicit then return end
    local build, install
    for config, this in self:each_config() do
        build = build or self:get('build', config)
        install = install or self:get('install', config)
    end
    if (not build or build and not build.type) and
       (not install or install and not install.type) then
        print_warning("a package '%s' is defined implicitly but has no build or install rules, "..
            "please check if the name is correct or create a rule for the '%s' "..
            "package to remove this warning%s", self.name, self.name, self:format_at())
    end
end

function P:check_undefined_uses()
    for config, this in self:each_config(true) do
        for spec in each(this.uses or {}) do
            local use = Target.from_use(spec)
            if not packages[use.name] then
                print_error("a package '%s' uses undefined package '%s'%s", self.name, use.name, self:format_at())
            end
        end
    end
end

function P:define_use(spec, context)
    local config, template = context.config, context.template
    local key = string.format('%s:%s:%s', spec, tostring(config), tostring(template))
    local cached, results, pkg = used_packages[key]
    if cached then return unpack(cached) end
    local target = Target.from_use(spec)
    local config = config or template and template.config
    if target.config == 'system' then -- skip those for now
        return
    end
    if target.config and target.config ~= config then
        pkg = P.define_package {
            name = target.name,
            config = target.config
        }
        results = { pkg, target.config }
    else
        pkg = P.define_package {
            name = target.name,
            config = config,
            template = template
        }
        results = { pkg, config }
    end
    used_packages[key] = results
    return unpack(results)
end

function P:process_config(config, this, template, rule)
    local build, install = this.build, this.install

    self:add_stage('export', config)

    if build.type == 'gradle-android' then
        build.in_source = true
        self.source = self.source or {}
        if self.source.ignore_dirty == nil then
            self.source.ignore_dirty = false
        end
        if build.toolchain == nil then
            build.toolchain = 'android-sdk-tools:host'
        end
    end

    if build.type then
        build.toolchain = self:gettoolchain(config)
    end

    if build.type == 'rust' then
        local rust_toolchain = build.rust_toolchain or 'stable'
        local name = string.format('rust-%s%s', rust_toolchain,
            build.system and '-'..build.system or '')
        P.define_package {
            name   = name,
            config = config,
            build = {
                type      = 'rust-toolchain',
                toolchain = 'rustup:host',
                name      = rust_toolchain,
                system    = build.system,
            }
        }
        self:add_require(name, { config = config })
        this.uses = append_uniq(name, this.uses)
        build.rust_toolchain = rust_toolchain
    end

    if not build.dir then
        if build.in_source then
            build.dir = '$pkg_source_dir'
        else
            build.dir = System.mkpath('${pkg_work_dir:?}', config)
        end
    end

    if build.in_source and self.source.ignore_dirty ~= false then
        if self.source:is_scm() then
            self.source.ignore_dirty = 'in_source'
        end
    end

    if build.type == 'gnu' then
        if build.generate or build.autoreconf then
            self:add_rule { 'autoreconf',
                { 'libtool', 'install', 'host' }
            }
            P.define_package { 'libtool', 'host' }
        end
    end

    if build.type == 'linux-module' or build.kernel_modules == true or
        install and install.modules then

        P.define_package { 'kernel', config }

        self:add_rule { 'configure', config,
            { 'kernel', 'configure', config }
        }
        self:add_rule { 'compile', config,
            { 'kernel', 'compile', config }
        }
        self:add_rule { 'install', config,
            { 'kernel', 'install', config }
        }
    elseif build.type then
        self:add_stage('configure', config)
        self:add_stage('compile', config)
    end

    if config == 'target' and build.target_requires_host then
        rule.requires = append(rule.requires or {}, { self.name, 'host' })
    end

    if install.type == nil and build and build.type then
        install.type = build.type
    end
    if install.type and install.type ~= false then
        self:add_stage('install', config)
    end

    for spec in each(self.requires) do
        self:add_require(spec, { config = config, template = template })
    end

    for spec in each(rule.requires) do
        local template = rule.requires.template or template
        self:add_require(spec, { config = config, template = template })
    end

    if this ~= self then
        for spec in each(this.uses or {}) do
            local use = Target.from_use(spec)
            P.define_package { use.name, use.config or config }
        end
    end

    -- Add configless stages to every config, then add rule-specific stages.
    local stages = extend(extend({}, self), rule)
    self:add_stages(stages, config, template)
end

function P.define_package(rule, context)
    rule = P:new(rule)

    if context then
        context.name = rule.name
        context.config = rule.config or context.config
        context.template = rule.template or context.template or {}
    else
        context = {
            name = rule.name,
            config = rule.config,
            template = rule.template or {},
            implicit = true
        }
    end
    if not context.config and context.template then
        context.config = context.template.config
    end
    push_context(context)
    rule.config, rule.template = nil, nil

    local pkg = packages[rule.name]
    if not pkg then
        pkg = P:new { rule.name }
        pkg.contexts = {}
        pkg.configs = {}
        pkg.build = {}
        pkg.install = {}
        pkg.export = {}
        pkg:add_stage('unpack')
        if pkg.name ~= 'patches' then
            pkg:add_stage('patch')
        end
        pkg:add_stage('export')
        local module, filename = find_module('pkg/'..rule.name)
        if module then
            table.merge(pkg, P:new(assert(module())))
            append(pkg.contexts, Context:new { filename = filename })
        end
        packages[rule.name] = pkg
    end
    append(pkg.contexts, context)

    for key in each { 'source', 'patches' } do
        if rule[key] then
            if pkg[key] == nil then pkg[key] = {} end
            table.merge(pkg[key], rule[key])
            rule[key] = nil
        end
    end

    local config, template, this = context.config, context.template, pkg
    if config then this = pkg:add_config(config) end

    rule = table.merge(copy(template), rule)
    table.merge(this, rule)

    if this ~= pkg then
        this.name, this.config = pkg.name, config
        if not getmetatable(this) then
            setmetatable(this, P)
        end
        if not this.build then this.build = {} end
        if not getmetatable(this.build) then
            setmetatable(this.build, { __index = pkg.build })
        end
        if not this.install then this.install = {} end
        if not getmetatable(this.install) then
            setmetatable(this.install, { __index = pkg.install })
        end
        if not this.export then this.export = {} end
        if not getmetatable(this.export) then
            setmetatable(this.export, { __index = pkg.export })
        end
    end

    if not pkg.source or not getmetatable(pkg.source) then
        pkg.source = Source:create(pkg.source, pkg.name)
        if pkg.patches and pkg.source:is_scm() then
            pkg.source.ignore_dirty = 'patches'
        end
    end

    if pkg.source and pkg.source.type == 'repo' then
        pkg:add_rule { 'unpack',
            { 'repo', 'install', 'host' }
        }
        P.define_package { 'repo', 'host' }
    end

    if config then
        pkg:process_config(config, this, template, rule)
    else
        for config, this in pkg:each_config() do
            pkg:process_config(config, this, template, rule)
        end
    end

    -- When custom stages are specified in configless rule add them to generic
    -- stages.
    if not config then
        pkg:add_stages(rule, config, template)
    end

    pop_context()

    return pkg
end

function package(rule, template)
    local context, level, info = { template = template }, 2
    repeat
        info = debug.getinfo(level, 'Sl')
        level = level+1
    until not info or info.what == 'main'
    if info then
        context.filename = info.source
        context.line = info.currentline
    end
    return P.define_package(rule, context)
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

function P.define_used_requires(requires)
    F.used_requires = {}
    local new_packages = {}
    for key, item in pairs(requires or {}) do
        local pkg, spec, context = item[1], item[2], item[3]
        local new_pkg = pkg:define_require(spec, context)
        new_packages[new_pkg.name] = new_pkg
    end
    return new_packages, F.used_requires
end

function P.process_rules(packages)
    local requires = {}
    while next(packages) do
        local patch_providers = {}
        for _, pkg in pairs(packages) do
            pkg.source:derive_properties(pkg.name)
            pkg:add_toolchain_requires()
            if pkg.patches then
                table.assign(patch_providers, pkg:add_patch_dependencies())
            end
        end
        packages, new_requires = P.define_used_requires(F.used_requires);
        table.assign(packages, patch_providers)
        table.assign(requires, new_requires)
    end
    return requires
end

function P.load_rules()
    local def_loader = lua_package.loaders[2]
    lua_package.loaders[2] = find_module

    packages, used_packages, F.used_requires = {}, {}, {}

    local function try_load_rules(dir)
        local filename = System.mkpath(dir, 'rules.lua')
        local file = io.open(filename, 'rb')
        if file then
            assert(loadstring(file:read('*a'), filename))()
            file:close()
        end
    end

    for dir in Command:new(quote(Jagen.cmd), 'get_path'):read('*a'):gmatch('[^\t\n]+') do
        try_load_rules(dir)
    end
    try_load_rules(System.mkpath(Jagen.project_dir))

    push_context({ implicit = true })

    local used_requires = F.used_requires
    local new_requires = P.process_rules(packages, used_requires)
    table.assign(used_requires, new_requires)
    new_packages = P.add_default_host_build_config(used_requires)
    P.process_rules(new_packages)

    for _, pkg in pairs(packages) do
        pkg:add_toolchain_uses()
        pkg:export_dirs()
        pkg:export_build_env()
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

    for name, pkg in pairs(packages) do
        pkg:check_build_configs()
        pkg:check_build_insource()
        pkg:check_build_toolchain()
        pkg:check_usages()
        pkg:check_undefined_uses()
    end

    lua_package.loaders[2] = def_loader

    return packages, not had_errors
end

return P
