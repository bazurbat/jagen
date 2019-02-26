local System = require 'System'
local Target = require 'Target'
local Source = require 'Source'
local Log    = require 'Log'
local Command = require 'Command'

local function setpkg(table, pkg)
    if pkg then 
        table[pkg.name] = pkg
    end
    return table
end

local P = {}
P.__index = P

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
        local filename, removed = self.filename:remove_prefix(System.dirname(Jagen.root_dir))
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

local RuleEngine = {}
RuleEngine.__index = RuleEngine

function RuleEngine:new()
    local engine = {
        packages              = {},
        _templates            = {},
        _variants             = {},
        has_rust_rules        = false,
        _define_count         = 0,
        _context_count        = 0,
        used_packages         = {},
        all_required_packages = {},
        required_packages     = {},
        required_specs        = {},
        had_errors            = false,
        had_warnings          = false,
    }
    setmetatable(engine, self)
    return engine
end

function RuleEngine:collect_require(pkg, spec, context, stage)
    local key = string.format('%s->%s^%s', spec, pkg.name, context and context:tokey() or '')
    if not self.all_required_packages[key] then
        local item = { pkg, spec, context }
        self.all_required_packages[key] = item
        self.required_packages[key] = item 
    end
    local key2 = string.format('%s^%s:%s^%s', spec, pkg.name,
        context.config or '', stage or '')
    if not self.required_specs[key2] then
        self.required_specs[key2] = { pkg, spec, context.config, stage }
    end
end

function RuleEngine:define_package(rule, context)
    P.rules._define_count = P.rules._define_count + 1

    if not context then
        context = Context:new {
            name = rule.name,
            config = rule.config,
            template = rule.template
        }
    end

    local config = rule.config or context.config
    local template = rule.template or context.template
    rule.config, rule.template = nil, nil

    if config and config ~= context.config or
       template and template ~= context.template then
        context = copy(context)
        context.config = config
        context.template = template
    end

    local pkg = P.rules.packages[rule.name]
    if not pkg then
        pkg = P:create(rule.name)
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
        local template = P.rules._templates[name]
        if template then
            table.merge(rule, template)
        else
            P.rules:print_warning("a package '%s' includes a template '%s' which is not defined\n--> %s", pkg.name, name, tostring(context))
        end
    end
    table.merge(rule, arg_rule)
    table.merge(this, rule)

    if config then
        local build, install = this.build, this.install

        if config == 'target' and build.target_requires_host then
            rule.requires = append_uniq(pkg.name..':host', rule.requires)
        end

        for spec in each(pkg.requires) do
            P.rules:collect_require(pkg, spec, context)
        end

        for spec in each(rule.requires) do
            local context = context
            if rule.requires.template ~= nil then
                context = copy(context or {})
                context.template = rule.requires.template
            end
            P.rules:collect_require(pkg, spec, context)
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
                P.rules:collect_require(pkg, spec, context, target.stage)
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

function RuleEngine:define_use(spec, context)
    local name, config = context.name, context.config
    local use = Target.from_use(spec)
    if use.name == name and use.config == config then
        Log.warning('a package specification %s is recursive in the context: %s', spec, tostring(context))
        return
    end
    local key
    if use.config and use.config ~= config then
        key = string.format('%s:%s', use.name, use.config)
        context = Context:new { name = name, config = config }
        config = use.config
    else
        key = string.format('%s:%s', use.name, context:tokey())
    end
    local fullkey = string.format('%s@%s', key, name)
    local pkg, seen = self.used_packages[key], self.used_packages[fullkey]
    if pkg then
        if not seen then append_uniq(context, pkg.contexts) end
    else
        pkg = self:define_package({ name = use.name, config = config }, context)
    end
    self.used_packages[key] = pkg self.used_packages[fullkey] = true
    return pkg
end

function RuleEngine:define_default_config(packages)
    local out = {}
    for _, pkg in pairs(packages) do
        local build = pkg.build
        if build and build.type and not next(pkg.configs) then
            local new
            if build.type == 'android-gradle' then
                new = self:define_package { name = pkg.name, config = 'target' }
            else
                new = self:define_package { name = pkg.name, config = 'host' }
            end
            setpkg(out, new)
        end
    end
    return out
end

function RuleEngine:define_variant(rule, context)
    local use = Target.from_use(rule.extends)
    if rule.name == use.name then
        self:print_error("a package '%s' extends itself\n--> %s", rule.name, tostring(context))
        return
    end
    if self.packages[rule.name] then
        self:print_error("can not define a variant package '%s', another package with the same name is already defined\n--> %s", rule.name, tostring(context))
        return
    end
    local pkg = self.packages[use.name]
    if not pkg then
        self:print_error("a package '%s' extends '%s' which is not defined\n--> %s", rule.name, use.name, tostring(context))
        return
    end
    if rule.config and not use.config then
        use.config = rule.config
    end
    if use.config and not pkg:has_config(use.config) then
        self:print_error("a package '%s' extends '%s:%s' but the package '%s' does not have a config '%s'\n--> %s", rule.name, use.name, use.config, use.name, use.config, tostring(context))
        return
    end
    pkg = copy(pkg)
    pkg.name = rule.name
    for this, _ in pkg:each_config(true) do
        for target in each(this.stages) do
            target.name = rule.name
            this.stages[target.stage] = target
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
    self.packages[rule.name] = pkg
    return self:define_package(rule, context)
end

function RuleEngine:define_variants()
    local out = {}
    for name, item in pairs(self._variants) do
        out[name] = P.define_variant(item[1], item[2])
    end
    return out
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

function RuleEngine:define_rust_packages(packages)
    local out = {}
    for _, pkg in pairs(packages) do
        for this, config in pkg:each_config() do
            local build = this.build
            if build.type == 'rust' then
                build.rust_toolchain = build.rust_toolchain or 'stable'
                build.system = build.system or pkg:_derive_rust_target(config)
                local name = string.format('rust-%s%s', build.rust_toolchain,
                    build.system and '-'..build.system or '')
                local rust_toolchain = self:define_package {
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
                out[name] = rust_toolchain
                self:collect_require(pkg, name, Context:new { name = pkg.name, config = config })
                this.uses = append_uniq(name, this.uses)
                self.has_rust_rules = true
            end
        end
    end
    return out
end

function RuleEngine:process_config(pkg, config, this)
    local new_packages = {}
    local build, install = this.build, this.install

    if build.type == 'android-gradle' then
        build.in_source = true
        pkg.source = pkg.source or {}
        if pkg.source.ignore_dirty == nil then
            pkg.source.ignore_dirty = false
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
        build.toolchain = pkg:gettoolchain(config)
    end

    local toolchain = build.toolchain
    if toolchain then
        self:collect_require(pkg, toolchain, Context:new { name = pkg.name, config = config })
        this.uses = append_uniq(toolchain, this.uses)
    end

    if not build.dir then
        if build.in_source then
            build.dir = '$pkg_source_dir'
        else
            build.dir = System.mkpath('${pkg_work_dir:?}', config)
        end
    end

    if build.in_source and pkg.source.ignore_dirty ~= false then
        if Source:is_known(pkg.source.type) then
            pkg.source.ignore_dirty = 'in_source'
        end
    end

    if build.type == 'gnu' then
        local generate, autoreconf = build.generate, build.autoreconf
        if generate == nil and autoreconf then
            generate = 'autoreconf'
        end
        if generate then
            pkg.build.generate = generate
            pkg:add_rule { 'generate' }
        end
        if rawget(build, 'generate') then
            rawset(build, 'generate', nil)
        end
    end

    if build.type == 'linux-module' or build.kernel_modules == true or
        install and install.modules then

        local p = self:define_package { name = 'kernel', config = config }
        new_packages[p.name] = p

        pkg:add_rule { 'configure', config,
            { 'kernel', 'configure', config }
        }
        pkg:add_rule { 'compile', config,
            { 'kernel', 'compile', config }
        }
        pkg:add_rule { 'install', config,
            { 'kernel', 'install', config }
        }
    end

    if build.type then
        pkg:add_stage('configure', config)
        pkg:add_stage('compile', config)
    end

    if install and install.type == nil and build and build.type then
        install.type = build.type
    end
    if install and install.type and install.type ~= false then
        pkg:add_stage('install', config)
    end

    return new_packages
end

function RuleEngine:process_source(pkg)
    if getmetatable(pkg.source) then return end
    local source, added = Source:create(pkg.source, pkg.name), {}
    if source.type == 'repo' then
        pkg:add_rule { 'unpack', { 'repo', 'install', 'host' } }
        local repo = self:define_package { name = 'repo', config = 'host' }
        added.repo = repo
    end
    if source:is_scm() then
        if pkg.patches then
            source.ignore_dirty = 'patches'
        end
        pkg.stages.unpack.stage = 'update'
    end
    pkg.source = source
    return added
end

function RuleEngine:define_project_package(project_dir)
    assert(project_dir)
    local name = project_dir:match('.*/([^/]+)')
    if not name then return end
    local exists, mkpath, build = System.file_exists, System.mkpath
    if exists(mkpath(project_dir, 'CMakeLists.txt')) then
        build = { type = 'cmake' }
    elseif exists(mkpath(project_dir, 'configure')) then
        build = { type = 'gnu' }
    elseif exists(mkpath(project_dir, 'autogen.sh')) then
        build = { type = 'gnu', generate = true }
    end
    if build then
        return P.rules:define_package(P:parse {
                name = name,
                source = '.',
                build = build,
                install = false
            })
    end
end

function RuleEngine:pass(packages)
    while next(packages) do
        local next_packages = {}
        for _, pkg in pairs(packages) do
            for_each(pkg._collected_targets, function(t) pkg:add_target(t) end)
            for this, config in pkg:each_config() do
                table.assign(next_packages, self:process_config(pkg, config, this))
                for_each(this._collected_targets, function(t) pkg:add_target(t) end)
                for spec in each(pkg.uses or {}, this.uses or {}) do
                    local added = self:define_use(spec, Context:new { name = pkg.name, config = config })
                    if added then
                        setpkg(self.packages, added)
                        next_packages[added.name] = added
                    end
                end
            end
        end
        local required = P.rules.required_packages
        P.rules.required_packages = {}
        for key, item in pairs(required) do
            local pkg, spec, context = item[1], item[2], item[3]
            local used = self:define_use(spec, context)
            if used then
                setpkg(self.packages, used)
                next_packages[used.name] = used
            end
        end
        packages = next_packages
    end
end

function RuleEngine:process_rules()
    local new

    self:pass(table.copy(self.packages))

    new = self:define_default_config(self.packages)
    table.assign(self.packages, new)
    self:pass(new)

    new = self:define_variants()
    table.assign(self.packages, new)
    self:pass(new)

    new = self:define_rust_packages(self.packages)
    table.assign(self.packages, new)
    self:pass(new)

    local new_packages
    repeat
        new_packages = {}
        for _, pkg in pairs(table.copy(self.packages)) do
            table.assign(new_packages, self:process_source(pkg))
        end
        self:pass(new_packages)
    until not next(new_packages)

    for _, pkg in pairs(self.packages) do
        for target in pkg:each() do
            for input in each(target.inputs) do
                if input.stage == 'unpack' then
                    local pkg = self.packages[input.name]
                    if pkg and pkg:is_scm() then
                        input.stage = 'update'
                    end
                elseif input.stage == 'update' then
                    local pkg = self.packages[input.name]
                    if pkg and not pkg:is_scm() then
                        input.stage = 'unpack'
                    end
                end
            end
        end
    end

    for key, item in pairs(self.required_specs) do
        local pkg, spec, config, stage = item[1], item[2], item[3], item[4]
        pkg:add_require_target(spec, config, stage)
    end

    for _, pkg in pairs(self.packages) do
        for this, config in pkg:each_config(true) do
            for spec in each(this.uses) do
                local use = Target.from_use(spec)
                local used = self.packages[use.name]
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

    for _, pkg in pairs(self.packages) do
        self._context_count = self._context_count + #pkg.contexts
    end

    -- print(string.format('defines: %d, contexts: %d', self._define_count, self._context_count))

    local source_exclude = os.getenv('jagen_source_exclude')
    local function is_scm(pkg)
        return pkg.source and pkg.source:is_scm()
    end
    if source_exclude then
        for item in string.gmatch(source_exclude, '%S+') do
            local invert = item:sub(1, 1) == '!'
            local shpat = invert and item:sub(2) or item
            if #shpat < 1 then
                Log.warning("invalid pattern '%s' in jagen_source_exclude list", item)
            end
            local luapat, match, matched = shpat:convert_pattern()
            for name, pkg in iter(P.rules.packages, filter(is_scm)) do
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
    end
end

function RuleEngine:print_error(...)
    Log.error(...)
    self.had_errors = true
end

function RuleEngine:print_warning(...)
    Log.warning(...)
    self.had_warnings = true
end

function RuleEngine:check_build_configs(pkg)
    if pkg.build and pkg.build.type and table.count(pkg.configs) == 0 then
        self:print_error("the package '%s' requires a build but has no configs defined",
            pkg.name)
    end
end

function RuleEngine:check_build_insource(pkg)
    local count, build = table.count(pkg.configs)
    for this, config in pkg:each_config() do
        build = this.build
        if build and build.in_source and build.in_source ~= 'multi' and count > 1 then
            self:print_warning("the package '%s' builds in source but has multiple configs defined, "..
                "please make sure that its build system supports this and set in_source='multi' "..
                "property to remove this warning",
                pkg.name)
            break
        end
    end
end

function RuleEngine:check_build_toolchain(pkg)
    local build
    for this, config in pkg:each_config() do
        build = this.build
        if build and build.type and build.toolchain == nil then
            self:print_error("the package '%s' requires '%s' build for "..
                "config '%s' but does not have a toolchain set", pkg.name,
                build.type, config)
        end
    end
end

function RuleEngine:check_undefined_uses(pkg)
    for this, config in pkg:each_config(true) do
        for spec in each(this.uses or {}) do
            local use = Target.from_use(spec)
            if not self.packages[use.name] then
                self:print_error("a package '%s' uses undefined package '%s'%s", pkg.name, use.name, pkg:format_at())
            end
        end
    end
end

-- TODO: investigate if this should be an error
-- the source is always defined as of now
function RuleEngine:check_usages(pkg)
    for this, config in pkg:each_config(true) do
        for spec in each(this.uses or {}) do
            local use = Target.from_use(spec)
            local pkg = self.packages[use.name]
            if pkg then
                local cfg = use.config or config
                local build, install = pkg:get('build', cfg), pkg:get('install', cfg)
                if (not build or build and not build.type) and
                   (not install or install and not install.type) and
                   not pkg.source then
                    self:print_warning("A package '%s' uses a package '%s' which does not have build, install or source "..
                        "rules defined. Possible reason could be incorrect package name spelling or missing pkg file.%s", pkg.name, pkg.name, pkg:format_at())
                end
            end
        end
    end
end

function RuleEngine:check()
    for name, pkg in pairs(self.packages) do
        self:check_build_configs(pkg)
        self:check_build_insource(pkg)
        self:check_build_toolchain(pkg)
        self:check_undefined_uses(pkg)
        self:check_usages(pkg)
    end
end

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

        for path in string.gmatch(package.path, '[^;]+') do
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
end)()

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
        local pkg = P.rules.packages[toolchain]
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
    local shared = { unpack = true, patch  = true, generate = true }
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

function P.collect_variants(rule, context)
    P.rules._variants[rule.name] = { rule, context }
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
    local req_pkg = P.rules.packages[use.name] assert(req_pkg)
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
        P.rules:print_warning('package %s requires patches which were not found: %s', self.name, table.concat(unresolved, ', '))
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
        P.rules:print_warning('could not find supplemental files for %s: %s', self.name, table.concat(unresolved, ', '))
    end
end

function P:add_ordering_dependencies()
    local prev, prev_clean, prev_cclean, common

    for curr in self:each() do
        if curr.stage == 'clean' then
            if prev_clean then
                curr.order_only = append(curr.order_only, prev_clean)
            end
            prev_clean = curr
        end
        if curr.stage == 'clean' and curr.config then
            prev_cclean = curr
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
            if prev_cclean then
                curr:append(prev_cclean)
                prev_cclean = nil
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
        local pkg = P.rules:define_package(rule, context)
        setpkg(P.rules.packages, pkg)
        return pkg
    end
end

function template(rule)
    rule = P:parse(rule)
    local name = rule.name
    rule.name = nil
    P.rules._templates[name] = rule
end

function P.load_rules()
    P.rules = RuleEngine:new()

    local prelude = [[
        local Log    = require 'Log'
        local System = require 'System'
    ]]

    local function try_load_rules(dir)
        local filename = System.mkpath(dir, 'rules.lua')
        local file = io.open(filename, 'rb')
        if file then
            assert(loadstring(prelude..file:read('*a'), filename))()
            file:close()
        end
    end

    for dir in each(Jagen:path()) do
        try_load_rules(dir)
    end
    try_load_rules(System.mkpath(Jagen.root_dir))

    local project_dir = os.getenv('jagen_project_dir')
    if project_dir then
        setpkg(P.rules.packages, P.rules:define_project_package(project_dir))
        local filename = System.mkpath(project_dir, '.jagen-rules.lua')
        local file = io.open(filename, 'rb')
        if file then
            assert(loadstring(file:read('*a'), filename))()
            file:close()
        end
    end

    P.rules:process_rules()
    P.rules:check()

    return P.rules.packages, not P.rules.had_errors
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
            for name, pkg in pairs(P.rules.packages) do
                for this, config in pkg:each_config() do
                    coroutine.yield(this, config, pkg)
                end
            end
        end)
end

return P
