require 'Package'

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
    local packages = {}

    local function add(rule)
        rule = Package:read(rule)
        local name = assert(rule.name)
        local qname = rule:qname()
        local pkg = packages[qname]
        if not pkg then
            pkg = Package:create(name)
            packages[qname] = pkg
            table.insert(packages, pkg)
        end
        table.merge(pkg, rule)
        pkg:add_build_targets(rule.config)
        for stage in each(rule) do
            pkg:add_target(Target:from_rule(stage, pkg.name, rule.config))
        end
    end

    for filename in each(import_paths('rules.lua')) do
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

    for _, pkg in ipairs(packages) do
        pkg.source = Source:create(pkg.source)
    end

    return packages
end
