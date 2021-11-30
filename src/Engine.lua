local Command  = require 'Command'
local Log      = require 'Log'
local Module   = require 'Module'
local Package  = require 'Package'
local Rule     = require 'Rule'
local System   = require 'System'
local Target   = require 'Target'

local format = string.format

local Engine = {}
local ProcessingEnv = {}

function Engine:new()
    local engine = {
        path = {},
        modules = {},
        packages  = {},
        templates = {},
    }
    setmetatable(engine, self)
    self.__index = self
    return engine
end

function ProcessingEnv:new(engine)
    return {
        engine    = engine,
        packages  = {},
        templates = {},
    }
end

function Engine:error(...)
    Log.error(...)
end

function Engine:load_rules()
    Log.debug1('load rules')

    local jagen_rules = System.mkpath(os.getenv('jagen_dir'), 'lib', 'rules.lua')
    local root_rules  = System.mkpath(os.getenv('jagen_root_dir'), 'rules.lua')

    local jagen = Module:load('jagen', jagen_rules)
    append(self.path, jagen:basename(jagen.filename))
    self:process_module(jagen)

    if System.file_exists(root_rules) then
        local root  = Module:load('root', root_rules)
        append(self.path, root:basename(root.filename))
        self:process_module(root)
    end

    local cmake = Command:new('cmake', '--version')
    if cmake:exists() then
        self.packages.jagen.cmake_version = cmake:match('^cmake version ([%w_.]+)$')
    end

    local Source = require 'Source'
    for pkg in each(self.packages) do
        if pkg.source ~= nil then
            pkg.source = Source:create(pkg.source, pkg.name)
        end
    end

    local ok, err
    ok, err = pcall(function ()
        for pkg in each(self.packages) do
            self:expand(pkg, pkg, pkg.name)
        end
    end)
    if not ok then
        Log.error(err.message)
        error('fatal error during rule expansion', 0)
    end

    if os.getenv('jagen_debug_engine') then
        for pkg in each(self.packages) do
            print(pretty(pkg))
        end
    end

    self:validate()

    return self.packages
end

function Engine:finalize()
    local path = {}
    for dir in each(self.path) do
        append(path, string.format('%s/pkg/?.sh', dir))
    end
    local script_path = table.concat(path, ';')

    for pkg in each(self.packages) do
        pkg._targets = {}
        for name, stage in pairs(pkg.stages or {}) do
            local target = Target.from_args(pkg.name, name)
            -- target.log = System.mkpath(self.packages.jagen.log_dir, target.ref..'.log')
            target.inputs = stage.inputs
            pkg._targets[name] = target
        end

        local filename, err = package.searchpath(pkg.name, script_path, '')
        if filename then
            pkg.backing_script = filename
        end
    end
end

function Engine:process_module(module, pkg)
    local env = ProcessingEnv:new(self)
    if pkg then
        self:add_package(pkg, env)
    end
    local modules = module:collect_unprocessed(self.modules)
    for mod in each(modules) do
        Log.debug1('process module %s', mod)
        for rule in each(mod.packages) do
            self:process_package(rule, env)
        end
        extend(env.templates, mod.templates)
        self.modules[mod.filename] = mod
    end
    for pkg in each(env.packages) do
        for spec in each(pkg.uses) do
            self:process_use(spec, pkg)
        end
    end
    self:apply_templates(env)
end

function Engine:process_package(rule, env)
    local pkg = self.packages[rule.ref]
    if pkg then
        Log.debug1('process package %s: merge with existing instance', rule.ref)
        pkg:merge(rule, { pkg = pkg })
    else
        Log.debug1('process package %s: add new instance', rule.ref)
        pkg = Package:new(rule.name, rule.config)
        pkg:merge(rule, { pkg = rule })
        self:add_package(pkg, env)
        local module = Module:load_package(rule, self.path)
        if module then
            self:process_module(module)
        end
    end
end

function Engine:process_use(spec, pkg)
    local target = Target.from_use(spec)
    local use = self.packages[target.ref]
    if use then
        Log.debug1('process use %s of %s: already processed', target.ref, pkg.ref)
    else
        Log.debug1('process use %s of %s: add new instance', target.ref, pkg.ref)
        use = Package:new(target.name, target.config)
        local module = Module:load_package(target, self.path)
        if module then
            self:process_module(module, use)
        else
            error(format("the package '%s' uses '%s' which is not defined"..
                "and not found in module search path", pkg.ref, target.ref))
        end
    end
end

function Engine:add_package(pkg, pass)
    self.packages[pkg.ref] = pkg
    append(pass.packages, pkg)
end

function Engine:apply_templates(pass)
    Log.debug1('%d new templates', #pass.templates)
    if next(pass.templates) then
        for pkg in each(self.packages) do
            if not pkg.abstract then
                Log.debug1('apply new templates to %s', pkg)
                for template in each(pass.templates) do
                    self:apply_template(template, pkg)
                end
            end
        end
        extend(self.templates, pass.templates)
    end
    Log.debug1('%d new packages', #pass.packages)
    if next(pass.packages) then
        for pkg in each(pass.packages) do
            if not pkg.abstract then
                Log.debug1('apply templates to %s', pkg)
                for template in each(self.templates) do
                    self:apply_template(template, pkg)
                end
                Log.debug1('end apply templates to %s', pkg)
            end
        end
        extend(self.packages, pass.packages)
    end
end

function Engine:apply_template(template, pkg)
    local state = {
        debug    = template.debug,
        packages = self.packages,
        matching = true,
        value    = {},
        i = 0, n = 1
    }
    if state.debug then
        Log.debug1('apply template %s', pretty(template))
    end
    if pkg:match(template.match, state) then
        state.matching = false
        for i = 1, state.n do
            state.i = i
            pkg:merge(template.apply, state)
            for spec in each(template.apply.uses) do
                if type(spec) == 'function' then
                    spec = spec(state)
                end
                self:process_use(spec, pkg)
            end
        end
    end
end

function Engine:expand(object, env, parent_key)
    local function sub(expr)
        local name, path = expr:match('([%w_]+):([%w_][%w_.]+)')
        local pkg
        if name then
            pkg = self.packages[name]
            if not pkg then
                error({
                    message = string.format("an expression '%s' in %s "..
                        "references a package '%s' which is not defined",
                        expr, parent_key, name),
                }, 0)
            end
        else
            path = expr
        end
        local keys = path:split2('.')
        local value
        if pkg then
            value = table.get(pkg, table.unpack(keys))
        else
            value = table.get(env, table.unpack(keys))
        end
        if value then
            return value
        else
            error({
                    message = string.format("an expansion of the expression "..
                        "'%s' in %s produced nil value", expr, parent_key)
                }, 0)
        end
    end

    if type(object) == 'table' then
        for key, value in pairs(object) do
            if type(key) == 'string' and key:sub(1, 1) ~= '_'
               or type(key) == 'number'
            then
                object[key] = self:expand(value, env, parent_key..'.'..key)
            end
        end
    elseif type(object) == 'string' then
        local count, depth, max_depth = 0, 0, 10
        repeat
            object, count = object:gsub('${([%w_][%w_.:]+)}', sub)
            depth = depth + 1
        until count == 0 or depth == max_depth
        if depth == max_depth then
            Log.warning("substitution depth limit %d reached while expanding "..
                "property %s, current value: %s", max_depth, parent_key, object)
        end
    end

    return object
end

function Engine:validate()
    local num_fatal = 0

    local function find_unexpanded(key, value)
        local tvalue = type(value)
        if tvalue == 'string' then
            if value:match('${.+}') then
                coroutine.yield(key, value)
            end
        elseif tvalue == 'table' then
            for k, v in pairs(value) do
                find_unexpanded(key..'.'..k, v)
            end
        end
    end

    local function iter_unexpanded(rule)
        return coroutine.wrap(function () find_unexpanded(rule.name, rule) end)
    end

    local unexpanded = {}

    for pkg in each(self.packages) do
        for key, value in iter_unexpanded(pkg) do
            unexpanded[key] = value
        end
    end

    if next(unexpanded) then
        num_fatal = num_fatal + 1
        Log.error('Unexpanded properties left after rule loading:')
        for key, value in pairs(unexpanded) do
            Log.error('  %s = %s', key, value)
        end
    end

    if num_fatal > 0 then
        error(string.format('Rule validation failed with %d fatal errors.', num_fatal), 0)
    end
end

return Engine
