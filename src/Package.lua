local System = require 'System'
local Target = require 'Target'
local Source = require 'Source'
local Log    = require 'Log'

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

local current_context
local context_stack = {}

local Context = {}

function Context:new(o)
    o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Context:__tostring(level)
    level = level or 0
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
        insert(lines, 1, string.rep('  ', level))
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
    return string.format('%s%s%s', self.name or '', sep or ':', self.config or '')
end

function P:format_contexts(start_col, start_1col)
    start_col = start_col or 0
    start_1col = start_1col or start_col
    local lines = {}
    for i = 1, #self.contexts do
        local context, level = self.contexts[i], 0
        while context do
            local str = context:__tostring(level)
            if #str > 0 then
                if i == 1 then
                    table.insert(lines, string.rep(' ', start_1col)..str)
                else
                    table.insert(lines, string.rep('  ', level)..str)
                end
            end
            context = context.parent
            level = level + 1
        end
    end
    return table.concat(lines, '\n'..string.rep(' ', start_col))
end

function P:format_last_context()
    return tostring(self.contexts[#self.contexts]) or '<unknown>'
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

    if type(rule.use) == 'string' then
        rule.use = { rule.use }
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

function P:add_require(spec, config, template)
	local req = Target.from_use(spec)
	local config = config or template and template.config
	if req.config == 'system' then -- skip those for now
		return
	end
	if req.config and req.config ~= config then
		return P.define_rule {
			name = req.name,
			config = req.config
		}, req.config
	else
		return P.define_rule {
			name = req.name,
			config = config,
			template = template
		}, config
	end
end

function P:gettoolchain(config)
    local host_toolchain = 'gcc-native'
    local target_toolchain = os.getenv('jagen_target_toolchain')
    local build, toolchain = self:get('build', config)
    if build then
        if build.toolchain ~= nil then
            toolchain = build.toolchain
        elseif build.type == 'rust' then
            toolchain = 'rust'
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

function P:add_toolchain(toolchain, config)
    local build = self:get('build', config)
    local stage = build and build.type and 'configure' or 'install'
    self:add_stage({ stage,
            { toolchain, 'install', config }
        }, config)
    push_context(table.merge(copy(current_context), {
                name = self.name,
                config = config
        }))
    local pkg = self:add_require(toolchain, config)
    pop_context()
    return pkg
end

function P:add_stage(rule, config)
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
            pkg:add_stage { name }
            stage = assert(pkg.stages[name])
        end
        stage.outputs = stage.outputs or {}
        table.iextend(stage.outputs, outputs)
        -- Adding patch files to arguments modifies the command line which is
        -- needed for Ninja to notice the changes in the list itself and rerun
        -- the command which then checks if the patches were indeed provided.
        stage.arg = sort(stage.outputs)

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
    local function export_build_dir(this, config)
        local export = this.export
        local build = this.build
        if build and export then
            export.build = rawget(export, 'build') or {}
            local dir = rawget(build, 'dir')
            if export.build.dir == nil then
                export.build.dir = dir
            end
            if export.dir == nil then
                export.dir = dir
            end
        end
    end
    export_build_dir(self)
    for config, this in self:each_config() do
        export_build_dir(this, config)
    end
    local export = self.export
    local source = self.source
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

function P:add_export_stages()
    if self.export and not self.stages['export'] then
        self:add_stage { 'export' }
    end
    for config, this in self:each_config() do
        if not this.stages then this.stages = {} end
        if this.export and not this.stages['export'] then
            local target = Target.from_args(self.name, 'export', config)
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

function P:each()
    return coroutine.wrap(function ()
            if self.stages then
                for target in each(self.stages) do
                    coroutine.yield(target, self)
                end
            end
            if self.configs then
                for config, this in pairs(self.configs) do
                    if this.stages then
                        for target in each(this.stages) do
                            coroutine.yield(target, this)
                        end
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
        print_error("the package '%s' requires a build but has no configs defined",
            self.name)
    end
end

function P:check_build_insource()
    local count, build = table.count(self.configs)
    for config, this in self:each_config() do
        build = this.build
        if build and build.in_source and count > 1 then
            print_error("the package '%s' requires an in source build but "..
                "has multiple configs defined -- this is not supported",
                self.name)
            break
        end
    end
end

function P:check_build_toolchain()
    local build
    for config, this in self:each_config() do
        build = this.build
        if build and build.type and not build.toolchain then
            print_error("the package '%s' requires '%s' build for "..
                "config '%s' but does not have a toolchain set", self.name,
                build.type, config)
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

    function add_toolchains(pkglist)
        local added = {}
        for _, pkg in pairs(pkglist) do
            for config, _ in pkg:each_config() do
                local toolchain = pkg:gettoolchain(config)
                if toolchain == 'rust' then
                    pkg:get('build', config).toolchain = toolchain
                    local use = pkg:get('use', config) or {}
                    append_uniq('rustup', use)
                    pkg:set('use', use, config)
                    append(added, pkg:add_toolchain('rustup', config))
                elseif toolchain then
                    pkg:get('build', config).toolchain = toolchain
                    local use = pkg:get('use', config) or {}
                    append_uniq(toolchain, use)
                    pkg:set('use', use, config)
                    append(added, pkg:add_toolchain(toolchain, config))
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

    for name, pkg in pairs(packages) do
        pkg:check_build_configs()
        pkg:check_build_insource()
        pkg:check_build_toolchain()
    end

    lua_package.loaders[2] = def_loader

    return packages, not (had_errors or had_warnings)
end

function P.define_rule(rule, context)
    rule = P:new(rule)

    local pkg = packages[rule.name]

    if not pkg then
        pkg = P:new { rule.name }
        pkg.contexts = {}
        pkg.configs = {}
        pkg.build = {}
        pkg.install = {}
        pkg.export = {}
        pkg:add_stage { 'unpack' }
        if pkg.name ~= 'patches' then
            pkg:add_stage { 'patch' }
        end
        local module, filename = find_module('pkg/'..rule.name)
        if module then
            table.merge(pkg, P:new(assert(module())))
            append(pkg.contexts, Context:new { filename = filename })
        end
        packages[rule.name] = pkg
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

    local template = {}
    if rule.template ~= false then
        template = rule.template or rule.pass_template or this.template or {}
    end

    rule.template, rule.pass_template = nil, nil
    table.merge(this, rule)
    this.template = template

    -- do not collect stage rules in the config
    table.iclean(this)

    if this ~= pkg then
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
            pkg.source.ignore_dirty = true
        end
    end

    push_context({
            name = pkg.name,
            config = config,
            implicit = true
        })

    if pkg.source and pkg.source.type == 'repo' then
        pkg:add_stage { 'unpack',
            { 'repo', 'install', 'host' }
        }
        P.define_rule { 'repo', 'host' }
    end

    if config then
        local build, install = this.build, this.install

        if build.toolchain and not template.build or
            template.build and template.build.toolchain == nil
        then
            template.build = template.build or {}
            template.build.toolchain = build.toolchain
        end

        if not build.dir then
            if build.in_source then
                build.dir = '$pkg_source_dir'
            else
                build.dir = System.mkpath('${pkg_work_dir:?}', config)
            end
        end

        if build.in_source then
            if pkg.source:is_scm() then
                pkg.source.ignore_dirty = true
            end
        end

        if build.type == 'gnu' then
            if build.generate or build.autoreconf then
                pkg:add_stage { 'autoreconf',
                    { 'libtool', 'install', 'host' }
                }
                P.define_rule { 'libtool', 'host' }
            end
        end

        if build.type == 'linux-module' or build.kernel_modules == true or
            install and install.modules then

            P.define_rule { 'kernel', config }

            pkg:add_stage({ 'configure',
                    { 'kernel', 'configure', config }
                }, config)
            pkg:add_stage({ 'compile',
                    { 'kernel', 'compile', config }
                }, config)
            pkg:add_stage({ 'install',
                    { 'kernel', 'install', config }
                }, config)
        elseif build.type then
            pkg:add_stage({ 'configure' }, config)
            pkg:add_stage({ 'compile' }, config)
        end

        if config == 'target' and build.target_requires_host then
            rule.requires = append(rule.requires or {}, { pkg.name, 'host' })
        end

        if install.type == nil and build and build.type then
            install.type = build.type
        end
        if install.type and install.type ~= false then
            pkg:add_stage({ 'install' }, config)
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
        for spec in each(stage.requires) do
            local req, config = pkg:add_require(spec, config, template)
            if req then
                append(stage, { req.name, 'install', config })
            end
        end
        pkg:add_stage(stage, config)
    end

    if context then pop_context() end

    if this ~= pkg then
        for spec in each(this.use or {}) do
            local use = Target.from_use(spec)
            P.define_rule { use.name, use.config or config }
        end
    end

    for spec in each(pkg.use or {}) do
        local use = Target.from_use(spec)
        if use.config or not config then
            P.define_rule { use.name, use.config }
        else
            local used = packages[use.name]
            if used then
                if table.count(used.configs) > 1 then
                    print_error(
"the %s uses %s without specifying a config but %s has multiple configs defined (%s), unable to determine which config to use\n"..
"    at %s:\n%s\n", tostring(this), spec, spec, table.concat(table.keys(used.configs), ', '),
                pkg:format_last_context(), pkg:format_contexts(6))
                else
                    P.define_rule { use.name, (next(used.configs)) }
                end
            else
                print_error(
"the %s uses %s without specifying a config but %s is not defined yet, unable to determine the config to use\n"..
"    at %s:\n%s\n", tostring(this), spec, spec,
                pkg:format_last_context(), pkg:format_contexts(6))
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
