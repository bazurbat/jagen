local System   = require 'System'
local Target   = require 'Target'
local Log      = require 'Log'
local Package  = require 'Package'
local Module   = require 'Module'
local Rule     = require 'Rule'

local Engine = {}
local Pass = {}

function Engine:new()
    local engine = {
        path = {},
        config = {},
        modules = {},
        packages  = {},
        templates = {},
        named_templates = {},
        final_templates = {},
        parse_templates = {}
    }
    setmetatable(engine, self)
    self.__index = self
    return engine
end

function Pass:new()
    return {
        packages  = {},
        templates = {},
        named_templates = {},
        parse_templates = {}
    }
end

function Engine:error(...)
    Log.error(...)
end

function Engine:load_rules()
    Log.debug2('load rules')

    local jagen_dir = os.getenv('jagen_dir')
    local root_dir = os.getenv('jagen_root_dir')

    local jagen = Module:load('jagen', System.mkpath(jagen_dir, 'lib', 'rules.lua'))
    local root_loaded, root  = pcall(Module.load, Module, 'root', System.mkpath(root_dir, 'rules.lua'))

    local toplevel_modules = {}
    extend(toplevel_modules, self:unprocessed_uses(jagen))
    if root_loaded then
        extend(toplevel_modules, self:unprocessed_uses(root))
    end

    for mod in each(toplevel_modules) do
        prepend(self.path, mod:basename(mod.filename))
    end

    local pass = Pass:new()
    self:process_modules(pass, toplevel_modules)

    local count = 0
    repeat
        count = count + 1
        Log.debug2('pass %d', count)

        self:apply_templates(pass)

        local uses = self:collect_unresolved_refs(pass)

        pass = Pass:new()
        for target in each(uses) do
            self:process_use(target, pass)
        end
    until not next(pass.packages) or count == 5

    local Source = require 'Source'
    for pkg in each(self.packages) do
        if pkg.source ~= nil then
            pkg.source = Source:create(pkg.source, pkg.name)
        end
    end

    local ok, err
    ok, err = pcall(function ()
        self:apply_named_templates()

        for key, config in pairs(self.config) do
            self:expand(config, config, config.name)
        end

        for pkg in each(self.packages) do
            self:expand(pkg, pkg, pkg.name)
        end
    end)
    if not ok then
        Log.error(err.message)
        error('fatal error during rule expansion', 0)
    end

    self:apply_final_templates()

    -- for _, config in pairs(self.config) do
    --     print(pretty(config))
    -- end

    -- for pkg in each(self.packages) do
    --     print(pretty(pkg))
    -- end

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
            target.log = System.mkpath(self.config.jagen.log_dir, target.ref..'.log')
            target.inputs = stage.inputs
            pkg._targets[name] = target
        end

        local filename, err = package.searchpath(pkg.name, script_path, '')
        if filename then
            pkg.backing_script = filename
        end
    end
end

function Engine:unprocessed_uses(module)
    local uses = module:collect_uses({ module })
    local new, seen = {}, extend({}, self.modules)

    for i = #uses, 1, -1 do
        local mod = uses[i]
        if not seen[mod.filename] then
            seen[mod.filename] = true
            append(new, mod)
        end
    end

    return new
end

function Engine:collect_unresolved_refs(pass)
    local global = self.packages
    local uses, seen = {}, {}

    for pkg in each(pass.packages) do
        for spec in each(pkg.uses or {}) do
            local use = Target.from_use(spec)
            if not global[use.ref] and not seen[use.ref] then
                seen[use.ref] = use
                append(seen, use.ref)
                append(uses, use)
            end
        end
    end

    if #uses == 0 then
        Log.debug2('0 unresolved references')
    else
        Log.debug2('%d unresolved use references: %s', #uses, table.concat(seen, ', '))
    end

    return uses
end

function Engine:add_package(pkg, pass)
    self.packages[pkg.ref] = pkg
    append(pass.packages, pkg)
end

function Engine:process_modules(pass, modules)
    for mod in each(modules) do
        self.modules[mod.filename] = true
        extend(self.parse_templates, mod.parse_templates)
    end

    for mod in each(modules) do
        Log.debug2('process module %s', mod)
        for rule in each(mod.named_templates) do
            self:process_named_template(rule, pass)
        end
        for rule in each(mod.configs) do
            self:process_config(rule, pass)
        end
        for rule in each(mod.packages) do
            self:process_package(rule, pass)
        end
        extend(pass.templates, mod.templates)
        extend(self.final_templates, mod.final_templates)
    end
end

function Engine:process_named_template(rule, pass)
    Log.debug2('process named template %s', rule.name)

    local key = rule.name
    local template = self.named_templates[key]
    if template then
        template:merge(rule)
    else
        self.named_templates[key] = rule
    end
end

function Engine:process_config(rule, pass)
    Log.debug2('process config %s', rule.name)

    local key = rule.name
    local config = self.config[key]
    if config then
        config:merge(rule)
    else
        self.config[key] = rule
    end
end

function Engine:process_package(rule, pass)
    for template in each(self.parse_templates) do
        self:apply_template(template, rule)
    end

    Log.debug2('process package %s', rule.ref)

    local pkg = self.packages[rule.ref]

    if pkg then
        pkg:merge(rule)
    else
        local module = Module:load_package(rule, self.path)
        if module then
            pkg = Package:new(rule.name, rule.config)
            pkg:merge(rule)
        else
            pkg = rule
        end

        self:add_package(pkg, pass)

        if module then
            self:process_modules(pass, self:unprocessed_uses(module))
        end
    end
end

function Engine:process_use(use, pass)
    Log.debug2('process use %s', use)
    local module = Module:load_package(use, self.path)
    if module then
        local pkg = Package:new(use.name, use.config)
        self:add_package(pkg, pass)
        self:process_modules(pass, self:unprocessed_uses(module))
    end
end

function Engine:apply_template(template, pkg)
    local state = { matching = true, value = {} }
    if pkg:match(template.match, state) then
        state.matching = false
        state.packages = self.packages
        if state.each then
            for i = 1, state.n do
                state.i = i
                pkg:merge(copy(template.apply), state)
            end
        else
            pkg:merge(copy(template.apply), state)
        end
    elseif template.match == nil then
        pkg:merge(copy(template.apply), state)
    end
end

function Engine:apply_templates(pass)
    Log.debug2('%d new templates', #pass.templates)
    if next(pass.templates) then
        for pkg in each(self.packages) do
            Log.debug2('[1] apply templates to %s', pkg)
            for template in each(pass.templates) do
                self:apply_template(template, pkg)
            end
        end
        extend(self.templates, pass.templates)
    end

    Log.debug2('%d new packages', #pass.packages)
    if next(pass.packages) then
        for pkg in each(pass.packages) do
            Log.debug2('[2] apply templates to %s', pkg)
            for template in each(self.templates) do
                self:apply_template(template, pkg)
            end
        end
        extend(self.packages, pass.packages)
    end
end

function Engine:apply_named_templates()
    for pkg in each(self.packages) do
        for name in each(pkg.apply) do
            local template = self.named_templates[name]
            if not template then
                error({ message = string.format("the package '%s' "..
                    "tries to apply template '%s' which is not defined",
                    pkg.name, name) }, 0)
            end
            Log.debug2('apply template %s to %s', name, pkg)
            self:apply_template(template, pkg)
        end
    end
end

function Engine:apply_final_templates()
    for pkg in each(self.packages) do
        Log.debug2('apply final templates to %s', pkg)
        for template in each(self.final_templates) do
            self:apply_template(template, pkg)
        end
    end
end

function Engine:expand(object, env, parent_key)
    local function sub(expr)
        local name, path = expr:match('([%w_]+):([%w_][%w_.]+)')
        local config
        if name then
            config = self.config[name]
            if not config then
                error({
                    message = string.format("an expression '%s' in %s "..
                        "references a config '%s' which is not defined",
                        expr, parent_key, name),
                }, 0)
            end
        else
            path = expr
        end
        local keys = path:split2('.')
        local value
        if config then
            value = table.get(config, table.unpack(keys))
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

    for config in each(self.config) do
        for key, value in iter_unexpanded(config) do
            unexpanded[key] = value
        end
    end
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
