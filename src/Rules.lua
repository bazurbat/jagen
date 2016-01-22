require 'Package'
local system = require 'system'

Rules = {}

function Rules.loadfile(filename)
    local rules = {}
    local env = {
        table = table,
        jagen = jagen
    }
    function env.package(rule)
        table.insert(rules, rule)
    end
    local chunk = loadfile(filename)
    if chunk then
        setfenv(chunk, env)
        chunk()
    end
    return rules
end

function Rules.load()
    local rules = {}

    local function add(rule)
        rule = Package:read(rule)
        local name = assert(rule.name)
        local qname = rule:qname()
        local pkg = rules[qname]
        if not pkg then
            pkg = Package:create(name)
            rules[qname] = pkg
        end
        table.merge(pkg, rule)
        pkg:add_build_targets(rule.config)
        for stage in each(rule) do
            pkg:add_target(Target:from_rule(stage, pkg.name, rule.config))
        end
    end

    for filename in each(system.import_paths('rules.lua')) do
        for rule in each(Rules.loadfile(filename)) do
            add(rule)
        end
    end

    add { 'toolchain', 'target', { 'install' } }

    if jagen.need_libtool then
        add { 'libtool', 'host' }
    end

    if jagen.need_repo then
        add { 'repo' }
    end

    for _, pkg in pairs(rules) do
        pkg.source = Source:create(pkg.source)
    end

    return rules
end

function Rules.merge(rules)
    local packages = {}
    for qname, rule in pairs(rules) do
        local name = assert(rule.name)
        local pkg = packages[name]
        if pkg then
            for target in each(rule.stages) do
                pkg:add_target(target)
            end
        else
            packages[rule.name] = rule
            table.insert(packages, rule)
        end

        local filename = system.mkpath(jagen.include_dir, rule:qname()..'.sh')
        local file = assert(io.open(filename, 'w+'))
        Script:write(rule, file)
        file:close()
    end
    return packages
end
