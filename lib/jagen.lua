require 'common'
require 'ninja'
local system = require 'system'
require 'target'

jagen = {}

function jagen.is_debug()
    return os.getenv('pkg_debug')
end

function jagen.message(...)
    print(string.format('\027[1;34m:::\027[0m %s', table.concat({...}, ' ')))
end
function jagen.warning(...)
    print(string.format('\027[1;33m:::\027[0m %s', table.concat({...}, ' ')))
end
function jagen.error(...)
    print(string.format('\027[1;31m:::\027[0m %s', table.concat({...}, ' ')))
end
function jagen.debug(...)
    if jagen.is_debug() then
        print(string.format('\027[1;36m:::\027[0m %s', table.concat({...}, ' ')))
    end
end
function jagen.debug1(...)
    if os.getenv('pkg_debug') == '1' then
        print(string.format('\027[1;36m:::\027[0m %s', table.concat({...}, ' ')))
    end
end
function jagen.debug2(...)
    if os.getenv('pkg_debug') == '2' then
        print(string.format('\027[1;36m:::\027[0m %s', table.concat({...}, ' ')))
    end
end

function jagen.flag(f)
    return false
end

function jagen.format_indent(n)
    local t = {}
    for i = 1, n do
        table.insert(t, " ")
    end
    return table.concat(t)
end

function read_package(rule)
    jagen.debug2('read_package: '..rule.name)
    local stages = {}
    for i, s in ipairs(rule) do
        table.insert(stages, s)
        rule[i] = nil
    end
    rule.stages = stages
    return rule
end

function load_rules()
    local rules = {}
    local sdk = os.getenv('pkg_sdk')

    if sdk then
        local lib_dir = os.getenv('pkg_lib_dir')
        local filename = system.mkpath(lib_dir, 'rules.'..sdk..'.lua')
        rules = dofile(filename)
    else
        return rules
    end

    for _, rule in ipairs(rules) do
        rules[rule.name] = rule
    end

    local function load_package(pkg_rule)
        local package = {}
        local tmp = {}
        local collected = {}

        local function load_source(source)
            if type(source) == 'string' then
                return { type = 'dist', location = source }
            else
                return source
            end
        end

        local function getkey(name, config)
            if config then
                return name .. ':' .. config
            else
                return name
            end
        end

        local function input_to_target(d)
            return target.new(d[1], d[2], d[3])
        end

        local function load_stage(stage_rule)
            local stage, config

            if type(stage_rule[1]) == 'string' then
                stage = stage_rule[1]
                table.remove(stage_rule, 1)
            end
            if type(stage_rule[1]) == 'string' then
                config = stage_rule[1]
                table.remove(stage_rule, 1)
            end

            local key = getkey(stage, config)
            local inputs = map(input_to_target, list(stage_rule))
            
            if tmp[key] then
                tmp[key].inputs = append(tmp[key].inputs or {}, inputs)
            else
                local target = target.new(pkg_rule.name, stage, config)
                target.inputs = inputs
                tmp[key] = target
                table.insert(collected, target)
            end
        end

        function add_previous(stages)
            local prev, common

            for _, s in ipairs(stages) do
                if prev then
                    if common and s.config ~= prev.config then
                        table.insert(s.inputs, 1, common)
                    else
                        table.insert(s.inputs, 1, prev)
                    end
                end

                prev = s
                if not s.config then
                    common = s
                end
            end
        end

        for_each(pkg_rule.stages, load_stage)
        add_previous(collected)

        package.name = pkg_rule.name
        package.source = load_source(pkg_rule.source)
        package.patches = pkg_rule.patches
        package.stages = collected

        -- print("===", package.name)
        -- pretty.dump(package)

        return package
    end

    local packages = map(load_package, rules)

    return packages
end

function jagen.generate_include_script(pkg)
    local name = pkg.name
    local dir = os.getenv('pkg_build_include_dir')
    local filename = system.mkpath(dir, name .. '.sh')

    local function source(pkg)
        local o = {}
        local s = pkg.source
        if s then
            if s.type == 'git' or s.type == 'hg' then
                table.insert(o, s.type)
                table.insert(o, s.location)
            elseif s.type == 'dist' then
                table.insert(o, system.mkpath('$pkg_dist_dir', s.location))
            end
        end
        return string.format('p_source="%s"\n', table.concat(o, ' '))
    end

    local function patches(pkg)
        local o = {}
        table.insert(o, 'pkg_patch_pre() {')
        for _, patch in ipairs(pkg.patches or {}) do
            local name = patch[1]
            local strip = patch[2]
            table.insert(o, string.format('  p_patch %d "%s"', strip, name))
        end
        table.insert(o, '}')
        return table.concat(o, '\n')
    end

    system.mkdir(dir)

    local f = assert(io.open(filename, 'w+'))
    f:write('#!/bin/sh\n')
    f:write(source(pkg))
    if pkg.patches then
        f:write(patches(pkg))
    end
    f:close()
end

command = arg[1]

function jagen.build(build_file, args)
    local build_command = system.mkpath(os.getenv('pkg_lib_dir'), 'build.sh')

    local rules = jagen.load_rules()

    local targets = map(target.new_from_arg, args)
    for k, t in pairs(targets) do
        if not t.stage then
            t.stage = 'build'
        end
        if not rules[t.name] then
            targets[k] = nil
        else
            for k, v in pairs(rules[t.name].stages) do
                print(k, v)
            end
        end
    end

    for _, t in pairs(targets) do
        print(t)
    end

    return 0 -- system.exec(build_command, unpack(targets))
end

if command == 'generate' then
    local build_file = arg[2]
    local rules_file = arg[3]
    local debug = os.getenv('')

    if system.file_older(build_file, rules_file) or jagen.is_debug() then
        jagen.message("Generating build rules")
        local packages = load_rules()
        ninja:generate(packages, arg[2], arg[3])
        for_each(packages, jagen.generate_include_script)
    end
elseif command == 'build' then
    local build_file = arg[2]
    local args = table.rest(arg, 3)

    return jagen.build(build_file, args)
end
