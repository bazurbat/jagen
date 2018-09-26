local System = require 'System'
local Target = require 'Target'
local Source = require 'Source'
local Log    = require 'Log'
local Command = require 'Command'

local P = {}
P.__index = P
P._templates = {}
P.has_rust_rules = false

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

local _define_count = 0
local _context_count = 0

local lua_package = package
local packages = {}
local used_packages = {}
local all_required_packages = {}
local required_packages = {}
local required_specs = {}

local current_context
local context_stack = {}

local Context = {}

function Context:new(o)
    o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Context:__rawtostring()
    local saved = Context.__tostring
    Context.__tostring = nil
    local s = tostring(self)
    Context.__tostring = saved
    return s
end

function Context:__tostring()
    local insert, concat = table.insert, table.concat
    local lines = {}
    local function append(...)
        for i = 1, select('#', ...) do
            insert(lines, (select(i, ...)))
        end
    end
    if self.name then
        append(self.name)
    end
    if self.config then
        append(':', self.config)
    end
    if self.filename then
        if #lines > 0 then append(' ') end
        if self.name or self.config then append('(') end
        local filename, removed = self.filename:remove_prefix(System.dirname(Jagen.project_dir))
        if removed then append('...') end append(filename)
        if self.line then append(':', self.line) end
        if self.name or self.config then append(')') end
    end
    if self.template then
        append(' [', concat(self.template, ', '), ']')
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

function Context:tokey(with_parent)
    local o = {}
    if with_parent then
        if self.name then
            table.insert(o, self.name)
        end
        if self.filename then
            table.insert(o, self.filename)
        end
        if self.line then
            table.insert(o, self.line)
        end
    end
    if self.config then
        table.insert(o, self.config)
    end
    if self.template then
        table.iextend(o, self.template)
    end
    return table.concat(o, ':')
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

local function find_module(name)
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

    for path in string.gmatch(lua_package.path, '[^;]+') do
        for name in each(names) do
            local filename = string.gsub(path, '%?', name)
            local file = io.open(filename, 'rb')
            if file then
                local module = assert(loadstring(assert(file:read('*a')), filename))
                file:close()
                return module, filename
            end
        end
    end
end

function P.init_rules()
    P._templates = {}
    P._variants = {}
    P.has_rust_rules = false
    _define_count = 0
    _context_count = 0
    packages = {}
    used_packages = {}
    all_required_packages = {}
    required_packages = {}
    required_specs = {}
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

    parse_section('build')
    parse_section('install')

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

function P:new(rule)
    rule = P:parse(rule)
    setmetatable(rule, self)
    return rule
end

function P:has_config(name)
    return self.configs and self.configs[name]
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

function P:get_build(key, config)
    return (self:get('build', config) or self.build)[key]
end

function P:get_toolchain_build(key, config)
    local toolchain = (self:get('build', config) or self.build).toolchain
    if toolchain then
        local pkg = packages[toolchain]
        if pkg then
            return pkg:get_build(key, config)
        end
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

function P:is_scm()
    return self.source and Source:is_known(self.source.type)
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

function P:add_target(target)
    local shared = { unpack = true, patch  = true, autoreconf = true }
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

function P:add_stage(name, config)
    return self:add_target(Target.from_args(self.name, name, config))
end

function P:add_rule(rule, config)
    return self:add_target(Target:parse(rule, self.name, config))
end

function P:collect_rule(rule, config)
    local target = Target:parse(rule, self.name, config)
    table.insert(self._collected_targets, target)
    return target
end

function P:collect_require(spec, context, stage)
    local key = string.format('%s:%s^%s', spec, self.name, context and context:tokey() or '')
    if not all_required_packages[key] then
        local item = { self, spec, context }
        all_required_packages[key] = item
        required_packages[key] = item 
    end
    local key2 = string.format('%s^%s:%s^%s', spec, self.name,
        context.config or '', stage or '')
    if not required_specs[key2] then
        required_specs[key2] = { self, spec, context.config, stage }
    end
end

function P.collect_variants(rule, context)
    P._variants[rule.name] = { rule, context }
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

function P:add_require_target(spec, config, stage)
    if not stage then
        local build, install = self:get('build', config), self:get('install', config)
        if build and build.type then
            stage = 'configure'
        elseif install and install.type then
            stage = 'install'
        else
            stage = 'install'
        end
    end
    local target = Target.from_args(self.name, stage, config)
    local use = Target.from_use(spec)
    local req_pkg = packages[use.name] assert(req_pkg)
    local req_target = req_pkg:add_required_stage(use.config or config)
    self:add_target(target:append(req_target))
end

function P:add_patch_dependencies()
    if not self.patches or not next(self.patches) then return end

    local function patch_names(pkg)
        local names = {}
        for item in each(pkg.patches) do
            append(names, string.format('%s', item[1]))
        end
        return names
    end

    local stage = self.stages['unpack']
    local names, unresolved = patch_names(self), {}
    for i, path in ipairs(self:find_files(names)) do
        if path ~= names[i] then
            stage.inputs = append_uniq(path, stage.inputs)
            -- Adding patch files to arguments modifies the command line which
            -- is needed for Ninja to notice the changes in the list itself and
            -- rerun the command.
            stage.arg = append_uniq(path, stage.arg)
            self.patches[i][3] = path
        else
            append(unresolved, path)
        end
    end
    if next(unresolved) then
        print_warning('package %s requires patches which were not found: %s', self.name, table.concat(unresolved, ', '))
    end
end

function P:add_files_dependencies()
    if not self.files or not next(self.files) then return end

    local function getnames()
        local o = {}
        for item in each(self.files) do
            append(o, string.format('%s', item[1]))
        end
        return o
    end

    local stage = self.stages['patch']
    local names, unresolved = getnames(), {}
    for i, path in ipairs(self:find_files(names)) do
        if path ~= names[i] then
            stage.inputs = append_uniq(path, stage.inputs)
            -- Adding patch files to arguments modifies the command line which
            -- is needed for Ninja to notice the changes in the list itself and
            -- rerun the command.
            stage.arg = append_uniq(path, stage.arg)
            self.files[i]._src_path = path
        else
            append(unresolved, path)
        end
    end
    if next(unresolved) then
        print_warning('could not find supplemental files for %s: %s', self.name, table.concat(unresolved, ', '))
    end
end

function P:add_ordering_dependencies()
    local prev, prev2, common

    for curr in self:each() do
        if curr.stage == 'clean' and curr.config then
            prev2 = curr
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
            if prev2 then
                curr:append(prev2)
                prev2 = nil
            end

            prev = curr
            if not curr.config then
                common = curr
            end
        end
    end
end

function P:export_build_env()
    local keys = { 'cc', 'cxx', 'arch', 'system', 'cpu',
                   'cflags', 'cxxflags', 'ldflags' }
    for this, config in self:each_config(true) do
        local build = this.build
        if rawget(build, 'cxxflags') == nil and rawget(build, 'cflags') ~= nil then
            build.cxxflags = build.cflags
        end
    end
    for this, config in self:each_config() do
        local build, export = this.build, this.export
        if build and export then
            for key in each(keys) do
                if rawget(export, key) == nil then
                    export[key] = rawget(build, key) or rawget(self.build, key)
                end
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
    for this, config in self:each_config() do
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

function P:check_build_configs()
    if self.build and self.build.type and table.count(self.configs) == 0 then
        print_error("the package '%s' requires a build but has no configs defined",
            self.name)
    end
end

function P:check_build_insource()
    local count, build = table.count(self.configs)
    for this, config in self:each_config() do
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
    for this, config in self:each_config() do
        build = this.build
        if build and build.type and build.toolchain == nil then
            print_error("the package '%s' requires '%s' build for "..
                "config '%s' but does not have a toolchain set", self.name,
                build.type, config)
        end
    end
end

function P:check_usages()
    for this, config in self:each_config(true) do
        for spec in each(this.uses or {}) do
            local use = Target.from_use(spec)
            local pkg = packages[use.name] assert(pkg)
            local cfg = use.config or config
            local build, install = pkg:get('build', cfg), pkg:get('install', cfg)
            if (not build or build and not build.type) and
               (not install or install and not install.type) and
               not pkg.source then
               print_warning("A package '%s' uses a package '%s' which does not have build, install or source "..
                   "rules defined. Possible reason could be incorrect package name spelling or missing pkg file.%s", self.name, pkg.name, self:format_at())
           end
        end
    end
end

function P:check_undefined_uses()
    for this, config in self:each_config(true) do
        for spec in each(this.uses or {}) do
            local use = Target.from_use(spec)
            if not packages[use.name] then
                print_error("a package '%s' uses undefined package '%s'%s", self.name, use.name, self:format_at())
            end
        end
    end
end

function P:create(name)
    local pkg = {
        name = name,
        stages = {},
        configs = {},
        source = {},
        build = {},
        install = {},
        export = {},
        contexts = {},
        _collected_targets = {},
    }
    setmetatable(pkg, self)
    pkg:add_stage('clean')
    pkg:add_stage('unpack')
    pkg:add_stage('patch')
    local module, filename = find_module(name)
    if module then
        table.merge(pkg, P:new(assert(module())))
    end
    return pkg
end

function package(rule)
    rule = P:parse(rule)
    local level, info = 2
    local context = Context:new { name = rule.name, config = rule.config, template = rule.template }
    repeat
        info = debug.getinfo(level, 'Sl')
        level = level+1
    until not info or info.what == 'main'
    if info then
        context.filename = info.source
        context.line = info.currentline
    end
    if rule.extends then
        return P.collect_variants(rule, context)
    else
        return P.define_package(rule, context)
    end
end

function P.define_package(rule, context)
    _define_count = _define_count + 1

    if not context then
        context = Context:new {
            name = rule.name,
            config = rule.config,
            template = rule.template
        }
    end

    local config = rule.config or context and context.config
    local template = rule.template or context and context.template
    rule.config, rule.template = nil, nil

    local pkg = packages[rule.name]
    if not pkg then
        pkg = P:create(rule.name)
        packages[rule.name] = pkg
    end
    if context then
        append_uniq(context, pkg.contexts)
    end

    local this = pkg
    if config then
        if not pkg.configs[config] then pkg.configs[config] = {} end
        this = pkg.configs[config]
        if not getmetatable(this) then
            this.name, this.config = pkg.name, config
            if not this.stages then this.stages = {} end
            if not this._collected_targets then this._collected_targets = {} end
            if not this.build then this.build = {} end
            if not this.install then this.install = {} end
            if not this.export then this.export = {} end
            setmetatable(this, P)
            setmetatable(this.build, { __index = pkg.build })
            setmetatable(this.install, { __index = pkg.install })
            setmetatable(this.export, { __index = pkg.export })
            pkg:add_stage('clean', config)
        end
    end

    if rule.source and Source:is_known(rule.source.type) then
        if rule.patches == nil then
            rule.patches = false
        end
    end
    -- merge source and patches to shared part regardless of rule context
    if rule.patches ~= nil then
        if rule.patches == false then
            pkg.patches = nil
        else
            pkg.patches = table.merge(pkg.patches or {}, rule.patches)
        end
        rule.patches = nil
    end
    if rule.source then
        pkg.source = table.merge(pkg.source or {}, rule.source)
        rule.source = nil
    end

    local arg_rule = rule
    rule = {}
    for name in each(template) do
        local template = P._templates[name]
        if template then
            table.merge(rule, template)
        else
            print_warning("a package '%s' includes a template '%s' which is not defined\n--> %s", pkg.name, name, tostring(context))
        end
    end
    table.merge(rule, arg_rule)
    table.merge(this, rule)

    if config then
        local build, install = this.build, this.install

        for spec in each(pkg.requires) do
            pkg:collect_require(spec, context)
        end

        for spec in each(rule.requires) do
            local context = context
            if rule.requires.template ~= nil then
                context = copy(context or {})
                context.template = rule.requires.template
            end
            pkg:collect_require(spec, context)
        end

        -- Add configless stages to every config, then add rule-specific stages.
        local stages = extend(extend({}, pkg), rule)
        for stage in each(stages) do
            local target = this:collect_rule(stage, config)
            for spec in each(stage.requires) do
                local context = context
                if stage.requires.template ~= nil then
                    context = copy(context or {})
                    context.template = stage.requires.template
                end
                pkg:collect_require(spec, context, target.stage)
            end
        end
    end

    -- When custom stages are specified in configless rule add them to generic
    -- stages.
    if not config then
        for stage in each(rule) do
            pkg:collect_rule(stage)
        end
    end

    return pkg
end

function P.define_variant(rule, context)
    local use = Target.from_use(rule.extends)
    if rule.name == use.name then
        print_error("a package '%s' extends itself\n--> %s", rule.name, tostring(context))
        return
    end
    if packages[rule.name] then
        print_error("can not define a variant package '%s', another package with the same name is already defined\n--> %s", rule.name, tostring(context))
        return
    end
    local pkg = packages[use.name]
    if not pkg then
        print_error("a package '%s' extends '%s' which is not defined\n--> %s", rule.name, use.name, tostring(context))
        return
    end
    if rule.config and not use.config then
        use.config = rule.config
    end
    if use.config and not pkg:has_config(use.config) then
        print_error("a package '%s' extends '%s:%s' but the package '%s' does not have a config '%s'\n--> %s", rule.name, use.name, use.config, use.name, use.config, tostring(context))
        return
    end
    pkg = copy(pkg)
    pkg.name = rule.name
    for this, _ in pkg:each_config(true) do
        for target in each(this.stages) do
            target.name = rule.name
        end
    end
    if use.config then
        for config in pairs(pkg.configs) do
            if config ~= use.config then
                pkg.configs[config] = nil
            end
        end
        if rule.config and rule.config ~= use.config then
            local this = pkg.configs[use.config]
            this.config = rule.config
            for target in each(this.stages) do
                target.config = rule.config
            end
            pkg.configs[rule.config] = this
            pkg.configs[use.config] = nil
        end
    end
    packages[rule.name] = pkg
    return P.define_package(rule, context)
end

function template(rule)
    rule = P:parse(rule)
    local name = rule.name
    rule.name = nil
    P._templates[name] = rule
end

function P:define_use(spec, context)
    local name, config = context.name, context.config
    local use = Target.from_use(spec)
    if use.name == name and use.config == config then
        Log.warning('a package specification %s is recursive in the context: %s', spec, tostring(context))
        return
    end
    local key
    if use.config and use.config ~= config then
        key = spec
        context = Context:new { name = name, config = config }
        config = use.config
    else
        key = string.format('%s:%s', use.name, context:tokey())
    end
    local fullkey = string.format('%s@%s', key, name)
    local pkg, seen = used_packages[key], used_packages[fullkey]
    if pkg then
        if not seen then append_uniq(context, pkg.contexts) end
    else
        pkg = P.define_package({ name = use.name, config = config }, context)
    end
    used_packages[key] = pkg used_packages[fullkey] = true
    return pkg
end

function P:process_source()
    if getmetatable(self.source) then return end
    local source, added = Source:create(self.source, self.name), {}
    if source.type == 'repo' then
        self:add_rule { 'unpack', { 'repo', 'install', 'host' } }
        local repo = P.define_package { name = 'repo', config = 'host' }
        added.repo = repo
    end
    if source:is_scm() then
        if self.patches then
            source.ignore_dirty = 'patches'
        end
        local unpack = self.stages['unpack']
        unpack.stage = 'update'
    end
    self.source = source
    return added
end

function P:process_config(config, this)
    local new_packages = {}
    local build, install = this.build, this.install

    if build.type == 'android-gradle' then
        build.in_source = true
        self.source = self.source or {}
        if self.source.ignore_dirty == nil then
            self.source.ignore_dirty = false
        end
        if build.toolchain == nil then
            build.toolchain = 'android-sdk-tools:host'
        end
        if build.profile == nil then
            build.profile = 'debug'
        end
        if build.clean == nil then
            build.clean = '$pkg_build_dir/app/build'
        end
    end

    if build.type then
        build.toolchain = self:gettoolchain(config)
    end

    local toolchain = build.toolchain
    if toolchain then
        self:collect_require(toolchain, Context:new { name = self.name, config = config })
        this.uses = append_uniq(toolchain, this.uses)
    end

    if not build.dir then
        if build.in_source then
            build.dir = '$pkg_source_dir'
        else
            build.dir = System.mkpath('${pkg_work_dir:?}', config)
        end
    end

    if build.in_source and self.source.ignore_dirty ~= false then
        if Source:is_known(self.source.type) then
            self.source.ignore_dirty = 'in_source'
        end
    end

    if build.type == 'gnu' then
        if build.generate or build.autoreconf then
            self:add_rule { 'autoreconf',
                { 'libtool', 'install', 'host' }
            }
            local p = P.define_package { name = 'libtool', config = 'host' }
            new_packages[p.name] = p
        end
    end

    if build.type == 'linux-module' or build.kernel_modules == true or
        install and install.modules then

        local p = P.define_package { name = 'kernel', config = config }
        new_packages[p.name] = p

        self:add_rule { 'configure', config,
            { 'kernel', 'configure', config }
        }
        self:add_rule { 'compile', config,
            { 'kernel', 'compile', config }
        }
        self:add_rule { 'install', config,
            { 'kernel', 'install', config }
        }
    end

    if build.type then
        self:add_stage('configure', config)
        self:add_stage('compile', config)
    end

    if install and install.type == nil and build and build.type then
        install.type = build.type
    end
    if install and install.type and install.type ~= false then
        self:add_stage('install', config)
    end

    if config == 'target' and build.target_requires_host then
        rule.requires = append(rule.requires or {}, { self.name, 'host' })
    end

    return new_packages
end

function P.process_rules(_packages)
    while next(_packages) do
        local new_packages = {}
        for _, pkg in pairs(_packages) do
            for_each(pkg._collected_targets, function(t) pkg:add_target(t) end)
            for this, config in pkg:each_config() do
                table.assign(new_packages, pkg:process_config(config, this))
                for_each(this._collected_targets, function(t) pkg:add_target(t) end)
                for spec in each(pkg.uses or {}, this.uses or {}) do
                    local added = pkg:define_use(spec, Context:new { name = pkg.name, config = config })
                    if added then
                        new_packages[added.name] = added
                    end
                end
            end
            for target in pkg:each() do
                for input in each(target.inputs) do
                    if input.stage == 'unpack' then
                        local pkg = packages[input.name]
                        if pkg and pkg:is_scm() then
                            input.stage = 'update'
                        end
                    elseif input.stage == 'update' then
                        local pkg = packages[input.name]
                        if pkg and not pkg:is_scm() then
                            input.stage = 'unpack'
                        end
                    end
                end
            end
        end
        local required = required_packages
        required_packages = {}
        for key, item in pairs(required) do
            local pkg, spec, context = item[1], item[2], item[3]
            local used = pkg:define_use(spec, context)
            if used then
                new_packages[used.name] = used
            end
        end
        _packages = new_packages
    end
end

function P.define_default_config()
    local new_packages = {}
    for _, pkg in pairs(table.copy(packages)) do
        local build = pkg.build
        if build and build.type and not next(pkg.configs) then
            local new_pkg
            if build.type == 'android-gradle' then
                new_pkg = P.define_package { name = pkg.name, config = 'target' }
            else
                new_pkg = P.define_package { name = pkg.name, config = 'host' }
            end
            new_packages[new_pkg.name] = new_pkg
        end
    end
    return new_packages
end

function P:_derive_rust_target(config)
    local system = self:get_toolchain_build('system', config)
    if not system then return end
    local triple = system:split('-')
    if #triple == 3 and triple[2] == 'linux' and not triple[3]:match('^android') then
        table.insert(triple, 2, 'unknown')
    end
    return table.concat(triple, '-')
end

function P.define_rust_packages()
    local new_packages = {}
    for _, pkg in pairs(packages) do
        for this, config in pkg:each_config() do
            local build = this.build
            if build.type == 'rust' then
                build.rust_toolchain = build.rust_toolchain or 'stable'
                build.system = build.system or pkg:_derive_rust_target(config)
                local name = string.format('rust-%s%s', build.rust_toolchain,
                    build.system and '-'..build.system or '')
                local rust_toolchain = P.define_package {
                    name   = name,
                    config = config,
                    build = {
                        type      = 'rust-toolchain',
                        toolchain = 'rustup:host',
                        name      = build.rust_toolchain,
                        system    = build.system
                    },
                    export = {
                        env = {
                            RUSTUP_HOME = '$rustup_env_RUSTUP_HOME',
                            CARGO_HOME = '$rustup_env_CARGO_HOME'
                        }
                    }
                }
                new_packages[name] = rust_toolchain
                pkg:collect_require(name, Context:new { name = pkg.name, config = config })
                this.uses = append_uniq(name, this.uses)
                P.has_rust_rules = true
            end
        end
    end
    return new_packages
end

function P.load_rules()
    local def_loader = lua_package.loaders[2]
    lua_package.loaders[2] = find_module

    P.init_rules()

    local function try_load_rules(dir)
        local filename = System.mkpath(dir, 'rules.lua')
        local file = io.open(filename, 'rb')
        if file then
            assert(loadstring(file:read('*a'), filename))()
            file:close()
        end
    end

    for dir in each(Jagen:path()) do
        try_load_rules(dir)
    end
    try_load_rules(System.mkpath(Jagen.project_dir))

    push_context({ implicit = true })

    P.process_rules(table.copy(packages))
    local new_packages = P.define_default_config()
    P.process_rules(new_packages)

    new_packages = {}
    for name, item in pairs(P._variants) do
        new_packages[name] = P.define_variant(item[1], item[2])
    end
    P.process_rules(new_packages)

    P.process_rules(P.define_rust_packages())

    repeat
        new_packages = {}
        for _, pkg in pairs(packages) do
            table.assign(new_packages, pkg:process_source())
        end
        P.process_rules(new_packages)
    until not next(new_packages)

    for key, item in pairs(required_specs) do
        local pkg, spec, config, stage = item[1], item[2], item[3], item[4]
        pkg:add_require_target(spec, config, stage)
    end

    for _, pkg in pairs(packages) do
        for this, config in pkg:each_config(true) do
            for spec in each(this.uses) do
                local use = Target.from_use(spec)
                local used = packages[use.name]
                if used then
                    for this, config in used:each_config(true) do
                        used:add_stage('export', config)
                    end
                end
            end
        end
        pkg:export_dirs()
        pkg:export_build_env()
    end

    for _, pkg in pairs(packages) do
        _context_count = _context_count + #pkg.contexts
    end

    -- print(string.format('defines: %d, contexts: %d', _define_count, _context_count))

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

function P:find_files(names)
    if not names then return end
    if next(names) then
        return Command:new(quote(Jagen.cmd), 'find_files', quote(self.name), quote(unpack(names))):aslist()
    else
        return {}
    end
end

function P:get_work_dir(config)
    return self:get('work_dir', config)
end

function P.all_configs()
    return coroutine.wrap(function ()
            for name, pkg in pairs(packages) do
                for this, config in pkg:each_config() do
                    coroutine.yield(this, config, pkg)
                end
            end
        end)
end

return P
