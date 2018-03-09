local System = require 'System'
local Target = require 'Target'

local P = {}

local function write_pkg_var(w, prefix, name, value)
    if not value then return end
    local tp = type(value)
    if tp == 'string' or tp == 'number' then
        w("pkg_%s%s='%s'", prefix, name, value)
    elseif tp == 'boolean' then
        w("pkg_%s%s='yes'", prefix, name)
    elseif tp == 'table' then
        local values = table.ivalues(value)
        if #values > 0 then
            w("pkg_%s%s='%s'", prefix, name, table.concat(values, '\n'))
        end
        local keys = sort(table.keys(value))
        for k in each(keys) do
            write_pkg_var(w, prefix..name, '_'..k, value[k])
        end
    else
        error(string.format('unable to write variable %s (%s)', name, tp))
    end
end

local function write_common(w, pkg)
    local function write_var(name, value)
        return write_pkg_var(w, '', name, value)
    end

    local predefined_keys = {
        'build',
        'config',
        'configs',
        'contexts',
        'export',
        'install',
        'name',
        'pass_template',
        'patches',
        'requires',
        'source',
        'stages',
        'template',
        'use',
    }
    local function custom_keys(_, key)
        return type(key) ~= 'number' and
            not find(function (k) return k == key end, predefined_keys)
    end

    local names = {}
    for k, v in iter(pkg, filter(custom_keys)) do
        table.insert(names, k)
    end
    table.sort(names)

    for name in each(names) do
        write_var(name, pkg[name])
    end

    local uses = {}
    for use in each(pkg.use) do
        local t = Target:from_use(use)
        append(uses, t.name)
    end
    write_var('use', uses)
end

local function write_source(w, pkg)
    local source = pkg.source
    if not source then return end

    local function write_var(name, value)
        return write_pkg_var(w, 'source_', name, value)
    end
    local names = sort(table.keys(source))

    if source.type and source.location then
        w("pkg_source='%s %s'", source.type, source.location)
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

local function write_export(w, pkg, config)
    local export = pkg.export
    if not export then return end
    local prefix = 'export_'
    if config then
        prefix = string.format('_%s__%s', config, prefix)
    end

    local function write_var(name, value)
        return write_pkg_var(w, prefix, name, value)
    end
    local names = sort(table.keys(export))

    for name in each(names) do
        write_var(name, export[name])
    end
end

local function generate_script(filename, pkg, config)
    local lines = {}
    local function w(format, ...)
        table.insert(lines, string.format(format, ...))
    end

    write_common(w, pkg)
    write_source(w, pkg)
    write_patches(w, pkg)
    -- write install first to allow referencing install dir from build options
    write_install(w, pkg)
    write_build(w, pkg)
    -- should be the last to allow referencing other variables
    write_export(w, pkg, config)

    if #lines > 0 then
        local file = assert(io.open(filename, 'w+'))
        file:write(table.concat(lines, '\n'), '\n')
        file:close()
    end
end

function P:generate(pkg, dir)
    local filename = System.mkpath(dir, string.format('%s.sh', pkg.name))
    generate_script(filename, pkg)
    for name, config in pairs(pkg.configs) do
        filename = System.mkpath(dir, string.format('%s__%s.sh', pkg.name, name))
        generate_script(filename, config, name)
    end
end

return P
