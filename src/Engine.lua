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
        parse_templates = {}
    }
end

function Engine:error(...)
    Log.error(...)
end

function Engine:load_rules()
    Log.debug2('load rules')

    local jagen = Module:load('jagen', System.mkpath(Jagen.dir, 'lib', 'rules.lua'))
    local root  = Module:load('root', System.mkpath(Jagen.root_dir, 'rules.lua'))

    local toplevel_modules = {}
    extend(toplevel_modules, self:unprocessed_uses(jagen))
    extend(toplevel_modules, self:unprocessed_uses(root))

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

    self:apply_final_templates()

    local Source = require 'Source'
    for pkg in each(self.packages) do
        if pkg.source ~= nil then
            pkg.source = Source:create(pkg.source, pkg.name)
        end
    end

    self.config.jagen.dir.root = os.getenv('jagen_root_dir')

    for pkg in each(self.packages) do
        Rule.expand(pkg, pkg)
    end

    for key, config in pairs(self.config) do
        Rule.expand(config, config)
    end

    for pkg in each(self.packages) do
        print(pretty(pkg))
    end

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
        for name, stage in pairs(pkg.stages) do
            local target = Target.from_args(pkg.name, name)
            target.log = System.mkpath(self.config.jagen.dir.log, target.ref..'.log')
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
        for config in each(mod.configs) do
            self.config[config.name] = config
        end
        self.modules[mod.filename] = true
        extend(self.parse_templates, mod.parse_templates)
    end

    for mod in each(modules) do
        Log.debug2('process module %s', mod)
        for rule in each(mod.packages) do
            self:process_package(rule, pass)
        end
        extend(pass.templates, mod.templates)
        extend(self.final_templates, mod.final_templates)
    end
end

function Engine:process_package(rule, pass)
    for template in each(self.parse_templates) do
        self:apply_template(template, rule)
    end

    Log.debug2('process package %s', rule.ref)

    local pkg = self.packages[rule.ref]

    if pkg then
        pkg:merge(pkg, rule)
    else
        local module = Module:load_package(rule, self.path)
        if module then
            pkg = Package:new(rule.name, rule.config)
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
    if pkg:match(pkg, template.match, state) then
        state.matching = false
        state.packages = self.packages
        if state.each then
            for i = 1, state.n do
                state.i = i
                pkg:merge(pkg, copy(template.apply), state)
            end
        else
            pkg:merge(pkg, copy(template.apply), state)
        end
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
            pkg:expand(pkg)
        end
        extend(self.packages, pass.packages)
    end
end

function Engine:apply_final_templates()
    for pkg in each(self.packages) do
        Log.debug2('apply final templates to %s', pkg)
        for template in each(self.final_templates) do
            self:apply_template(template, pkg)
        end
        pkg:expand(pkg)
    end
end

return Engine
