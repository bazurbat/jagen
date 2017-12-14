local System = require 'System'

local P = {}

local function write_pkg_var(w, prefix, name, value)
    if not value then return end
    local tp = type(value)
    if tp == 'string' or tp == 'number' then
        w('pkg_%s%s="%s"', prefix, name, value)
    elseif tp == 'boolean' then
        w("pkg_%s%s='yes'", prefix, name)
    elseif tp == 'table' then
        w('pkg_%s%s="%s"', prefix, name, table.concat(value, '\n'))
    else
        error(string.format('unable to write variable %s (%s)', name, tp))
    end
end

local function write_env(w, pkg)
    local env = pkg.env or { pkg.config }
    for _, e in ipairs(env) do
        w('use_env %s || true', e)
    end
end

local function write_common(w, pkg)
    local function write_var(name, value)
        return write_pkg_var(w, '', name, value)
    end
    write_var('work_dir', pkg.work_dir)
end

local function write_source(w, pkg)
    local source = pkg.source
    if not source then return end

    local function write_var(name, value)
        return write_pkg_var(w, 'source_', name, value)
    end
    local names = sort(table.keys(source))

    if source.type and source.location then
        w('pkg_source="%s %s"', source.type, source.location)
    end

    for name in each(names) do
        write_var(name, source[name])
    end
end

local function write_patches(w, pkg)
    local patches = pkg.patches
    if not patches then return end

    local function write_var(name, value)
        return write_pkg_var(w, 'patches_', name, value)
    end
    local names = sort(table.keys(patches))

    for name in each(names) do
        write_var(name, patches[name])
    end

    if #patches > 0 then
        assert(patches.required and #patches == #patches.required)
        w('jagen_pkg_apply_patches() {')
        for i, item in ipairs(patches) do
            local name = item[1]
            local strip = item[2]
            w('  pkg_run_patch %d "%s"', strip, patches.required[i])
        end
        w('}')
    end
end

local function write_build(w, pkg)
    local build = pkg.build
    if not build then return end

    local function write_var(name, value)
        return write_pkg_var(w, 'build_', name, value)
    end
    local names = sort(table.keys(build))

    for name in each(names) do
        write_var(name, build[name])
    end
end

local function write_install(w, pkg)
    local install = pkg.install
    if not install then return end

    local function write_var(name, value)
        return write_pkg_var(w, 'install_', name, value)
    end
    local names = sort(table.keys(install))

    for name in each(names) do
        write_var(name, install[name])
    end
end

local function generate_script(filename, pkg)
    local file = assert(io.open(filename, 'w+'))

    local function w(format, ...)
        file:write(string.format(format..'\n', ...))
    end

    write_env(w, pkg)
    write_common(w, pkg)
    write_source(w, pkg)
    write_patches(w, pkg)
    -- write install first to allow referencing install dir from build options
    write_install(w, pkg)
    write_build(w, pkg)

    file:close()
end

function P:generate(pkg, dir)
    local filename = System.mkpath(dir, string.format('%s.sh', pkg.name))
    generate_script(filename, pkg)
    for name, config in pairs(pkg.configs) do
        filename = System.mkpath(dir, string.format('%s__%s.sh', pkg.name, name))
        generate_script(filename, config)
    end
end

return P
