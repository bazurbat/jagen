local system = require 'system'

local P = {}

function P:write(script, name)
    if script and #script > 0 then
        local filename = system.mkpath(
            jagen.include_dir, string.format("%s.sh", name))
        local file = assert(io.open(filename, 'w+'))
        file:write(table.concat(script, '\n'))
        file:close()
    end
end

function P:get_shared(pkg)
    local o = {}
    local function w(format, ...)
        table.insert(o, string.format(format, ...))
    end

    local source = pkg.source

    if source.type and source.location then
        w('pkg_source="%s %s"', source.type, source.location)
    end
    if source.branch then
        w('pkg_source_branch="%s"', source.branch)
    end
    if source.path then
        w('pkg_source_dir="%s"', source.path)
    end

    if pkg.patches then
        w('jagen_pkg_apply_patches() {')
        for _, patch in ipairs(pkg.patches or {}) do
            local name = patch[1]
            local strip = patch[2]
            w('  pkg_run_patch %d "%s"', strip, name)
        end
        w('}')
    end

    if pkg.build then
        local build = pkg.build
        if build.type then
            w("pkg_build_type='%s'", build.type)
        end
        if build.generate then
            w("pkg_build_generate='yes'")
        end
    end

    return o
end

function P:get(pkg)
    local o = {}
    local function w(format, ...)
        table.insert(o, string.format(format, ...))
    end

    local env = pkg.env or { pkg.config }
    for _, e in ipairs(env or {}) do
        w('use_env %s || return', e)
    end

    -- put install variables before build to allow referencing them from
    -- configure options
    if pkg.install then
        local i = pkg.install
        if i.path then
            w('pkg_dest_dir="%s"', i.path)
        end
        if i.prefix then
            w('pkg_prefix="%s"', i.prefix)
        end
    end

    local build_dir

    if pkg.build then
        local build = pkg.build

        if build.profile then
            w("pkg_build_profile='%s'", build.profile)
        end

        if build.options then
            local o = build.options
            if type(build.options) == 'string' then
                o = { build.options }
            end
            w('pkg_options="%s"', table.concat(o, '\n'))
        end
        if build.libs then
            w("pkg_libs='%s'", table.concat(build.libs, ' '))
        end
        if build.work_dir then
            w('pkg_work_dir="%s"', build.work_dir)
        end
        if build.in_source then
            build_dir = '$pkg_source_dir'
        end
        if build.directory then
            build_dir = build.directory
        end
    end

    if build_dir then
        w('pkg_build_dir="%s"', build_dir)
    end

    return o
end

return P
