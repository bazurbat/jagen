local System = require 'System'
local Target = require 'Target'
local Rules = require 'Rules'
local Config = require 'Config'

local P = {}

local function write_var(w, name, value)
    name = name:to_identifier()
    local typ = type(value)
    if typ == 'string' or typ == 'number' then
        w("%s='%s'", name, value)
    elseif typ == 'boolean' then
        w("%s='yes'", name, value)
    elseif typ == 'table' then
        if #value > 0 then
            w("%s='%s'", name, table.concat(value, '\t'))
        else
            for k, v in pairs(value) do
                write_var(w, name..'_'..k, v)
            end
        end
    end
end

local function write_pkg_var(w, prefix, name, value)
    if not value then return end
    local tp = type(value)
    if tp == 'string' or tp == 'number' then
        w("pkg_%s%s='%s'", prefix, name, value)
    elseif tp == 'boolean' then
        w("pkg_%s%s='yes'", prefix, name)
    elseif tp == 'table' then
        local values = table.ivalues(table.filter(value, function (v) return v end))
        if #values > 0 then
            w("pkg_%s%s='%s'", prefix, name, table.concat(values, '\t'))
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
        'files',
        'install',
        'name',
        'patches',
        'requires',
        'source',
        'stages',
        'uses',
        '_pkg',
        '_collected_targets',
        '_config',
        '_base'
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

    local resolved = filter(function(item) return item[3] end, patches)
    if #resolved > 0 then
        w('jagen_stage_apply_patches() {')
            for i, item in ipairs(resolved) do
                local name, n, path = item[1], item[2], item[3]
                w('  pkg_run_patch %d "%s"', n, path)
            end
        w('}')
    end
end

local function write_files(w, pkg)
    local files = pkg.files
    if not files then return end

    local function write_var(name, value)
        return write_pkg_var(w, 'files_', name, value)
    end

    for i = 1, #files do
        local item = files[i]
        if item._src_path then
            write_var(i, { item[1], item._src_path, item.path })
        end
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
    local prefix = 'export__'
    if config then
        prefix = string.format('%s%s_', prefix, config)
    end

    local function write_var(name, value)
        return write_pkg_var(w, prefix, name, value)
    end
    local names = sort(table.keys(export))

    for name in each(names) do
        write_var(name, export[name])
    end
end

local function write_uses(w, pkg, config)
    local uses = pkg.uses
    if not uses then return end
    prefix = ''
    if config then
        prefix = string.format('_%s__', config)
    end
    local function write(name, value)
        return write_pkg_var(w, prefix, name, value)
    end

    local function write_use(name, config, pkg)
        for key, val in pairs(pkg.export) do
            write_var(w, name..'_'..key, val)
        end
        if config then
            for key, val in pairs(pkg:get('export', config)) do
                write_var(w, name..'_'..key, val)
            end
        end
    end

    for spec in each(uses) do
        local use = Target.from_use(spec)
        local pkg = Rules.rules.packages[use.name]
        write_use(use.name, config, pkg)
        if use.alias then
            write_use(use.alias, config, pkg)
        end
    end

    -- local names, aliases, targets = {}, {}, sort(map(Target.from_use, uses),
    --     function (a, b) return a.name < b.name end)
    -- for target in each(targets) do
    --     append(names, tostring(target))
    --     if target.alias then
    --         append(aliases, string.format('%s=%s',
    --                 string.to_identifier(target.alias),
    --             string.to_identifier(target.name)))
    --     end
    -- end

    write('uses', names)
    write('use_alias', aliases)
end

local function write_config(w, pkg, config)
    if not config then return end
    local metatable = getmetatable(config)
    if metatable then
        write_config(w, pkg, metatable.__index)
    end
    for key, value in pairs(config) do
        if type(key) == 'string' and key:sub(1, 1) ~= '_' then
            write_var(w, 'pkg_'..key, pkg:expand(copy(value)))
        end
    end
end

local function generate_script(filename, pkg, config)
    local lines = {}
    local function w(format, ...)
        table.insert(lines, string.format(format, ...))
    end

    -- write_config(w, pkg, pkg._config)
    write_common(w, pkg)
    write_source(w, pkg)
    write_patches(w, pkg)
    write_files(w, pkg)
    -- write install first to allow referencing install dir from build options
    write_install(w, pkg)
    write_build(w, pkg)
    -- should be the last to allow referencing other variables
    write_export(w, pkg, config)
    write_uses(w, pkg, config)

    if pkg.env then
        w('')
    end
    for key, value in Config.each(pkg.env or {}) do
        w("export %s='%s'", key, value)
    end

    local file = assert(io.open(filename, 'w+'))
    file:write(table.concat(lines, '\n'), '\n')
    file:close()
end

function P:generate(pkg, dir)
    local filename = System.mkpath(dir, string.format('%s.sh', pkg.name))
    generate_script(filename, pkg)
    for name, config in pairs(pkg.configs) do
        filename = System.mkpath(dir, string.format('%s:%s.sh', pkg.name, name))
        generate_script(filename, config, name)
    end
end

return P
