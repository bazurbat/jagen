require 'common'
require 'ninja'
require 'rules'
require 'system'
require 'target'

Jagen = {}

function Jagen.message(...)
    print(string.format('\027[1;34m:::\027[0m %s', table.concat({...}, ' ')))
end
function Jagen.warning(...)
    print(string.format('\027[1;33m:::\027[0m %s', table.concat({...}, ' ')))
end
function Jagen.error(...)
    print(string.format('\027[1;31m:::\027[0m %s', table.concat({...}, ' ')))
end
function Jagen.debug(...)
    if env('pkg_debug') == 'yes' then
        print(string.format('\027[1;36m:::\027[0m %s', table.concat({...}, ' ')))
    end
end

function Jagen.flag(f)
    return false
end

function Jagen.format_indent(n)
    local t = {}
    for i = 1, n do
        table.insert(t, " ")
    end
    return table.concat(t)
end

function Jagen.generate_include_script(pkg)
    local name = pkg.name
    local dir = env('pkg_build_include_dir')
    local filename = mkpath(dir, name .. '.sh')

    local function source(pkg)
        local o = {}
        local s = pkg.source
        if s then
            if s.type == 'git' or s.type == 'hg' then
                table.insert(o, s.type)
                table.insert(o, s.location)
            elseif s.type == 'dist' then
                table.insert(o, mkpath('$pkg_dist_dir', s.location))
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

    Jagen.mkdir(dir)

    local f = assert(io.open(filename, 'w+'))
    f:write('#!/bin/sh\n')
    f:write(source(pkg))
    if pkg.patches then
        f:write(patches(pkg))
    end
    f:close()
end

command = arg[1]

function Jagen.build(build_file, args)
    local targets = map(tostring, map(Target.new_from_arg, args))
    local build_command = mkpath(env('pkg_lib_dir'), 'build.sh')

    return exec(build_command, unpack(targets))
end

if command == 'generate' then
    local build_file = arg[2]
    local rules_file = arg[3]

    if file_older(build_file, rules_file) then
        Jagen.message("Generating build rules")
        local packages = load_rules(arg[3])
        Ninja:generate(packages, arg[2], arg[3])
        for_each(packages, Jagen.generate_include_script)
    end
elseif command == 'build' then
    local build_file = arg[2]
    local args = table.rest(arg, 3)

    return Jagen.build(build_file, args)
end
