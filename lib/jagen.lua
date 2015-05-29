require 'common'
require 'ninja'
require 'rules'
local system = require 'system'
require 'target'

jagen = {}

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
    if os.getenv('pkg_debug') == 'yes' then
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
    local targets = map(tostring, map(target.new_from_arg, args))
    local build_command = system.mkpath(os.getenv('pkg_lib_dir'), 'build.sh')

    return system.exec(build_command, unpack(targets))
end

if command == 'generate' then
    local build_file = arg[2]
    local rules_file = arg[3]

    if system.file_older(build_file, rules_file) then
        jagen.message("Generating build rules")
        local packages = load_rules(arg[3])
        Ninja:generate(packages, arg[2], arg[3])
        for_each(packages, jagen.generate_include_script)
    end
elseif command == 'build' then
    local build_file = arg[2]
    local args = table.rest(arg, 3)

    return jagen.build(build_file, args)
end
