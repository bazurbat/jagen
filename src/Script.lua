local Chunk  = require 'Chunk'
local Module = require 'Module'
local System = require 'System'
local Target = require 'Target'

local P = {}

local function is_simple_type(t)
    return t == 'string' or t == 'number' or t == 'boolean'
end

local function is_table_type(t)
    return t == 'table'
end

local function is_simple_value(value)
    local t = type(value)
    return t == 'string' or t == 'number' or t == 'boolean'
end

local function write_array(value)
    local t = type(value)
    if is_simple_type(t) then
        return tostring(value)
    elseif is_table_type(t) then
        local result = {}
        for i, v in ipairs(value) do
            append(result, write_array(v))
        end
        return table.concat(result, '\t')
    end
end

local function write(fmt, name, value)
    local tvalue = type(value)
    name = string.to_identifier(name)
    if tvalue == 'string' then
        fmt("%s='%s'", name, value)
    elseif tvalue == 'number' then
        fmt("%s='%s'", name, tostring(value))
    elseif tvalue == 'boolean' and value then
        fmt("%s='true'", name)
    elseif tvalue == 'table' then
        local keys = table.keys(value)
        table.sort(keys)
        for key in each(keys) do
            local newname = key
            if #name > 0 then
                newname = string.format('%s_%s', name, key)
            end
            write(fmt, newname, value[key])
        end
        if #value > 0 then
            local s = write_array(value)
            if not string.empty(s) then
                fmt("%s='%s'", name, s)
            end
        end
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

function P:write(pkg, filename, engine)
    local skip = { stage = true, import = true, export = true, env = true }

    local properties = {}
    for key, val in pairs(pkg) do
        if key:sub(1, 1) ~= '_' and type(val) == 'string' then
            append(properties, key)
            skip[key] = true
        end
    end
    table.sort(properties)

    local standard_sections = {
        source = true, toolchain = true, build = true, install = true,
        'source', 'toolchain', 'build', 'install'
    }
    local other_sections = {}

    for key, val in pairs(pkg) do
        if not skip[key]
                and not standard_sections[key]
                and key:sub(1, 1) ~= '_' then
            append(other_sections, key)
        end
    end

    local main, exports, imports, unset, env = {}, {}, {}, {}, {}

    local function add_main(s, ...)
        append(main, string.format(s, ...))
    end
    local function add_export(s, ...)
        append(exports, string.format(s, ...))
    end
    local function add_import(s, ...)
        append(imports, string.format(s, ...))
    end
    local function add_env(s, ...)
        append(env, string.format(s, ...))
    end

    for key in each(properties) do
        write(add_main, key, pkg[key])
    end

    for name in each(standard_sections) do
        if pkg[name] ~= nil then
            write(add_main, name, pkg[name])
        end
    end

    for name in each(other_sections) do
        write(add_main, name, pkg[name])
    end

    local stages = table.keys(pkg.stage)
    for stage in each(stages) do
        write(add_main, 'stage_'..stage, pkg.stage[stage])
    end

    local function collect_env(env)
        for key, value in pairs(env) do
            if value == '<unset>' then
                append(unset, string.format('unset %s', key))
            else
                write(add_env, key, value)
            end
        end
    end

    for name in each(pkg.uses) do
        local use = engine.packages[name]
        if use and use.export and use.export.env then
            collect_env(use.export.env)
        end
    end

    if pkg.env ~= nil then
        collect_env(pkg.env)
    end

    if pkg.export ~= nil then
        write(add_export, '', pkg.export)
    end
    if pkg.import ~= nil then
        write(add_import, '', pkg.import)
    end

    table.sort(exports)
    table.sort(imports)
    table.sort(unset)
    table.sort(env)

    local prefix = 'pkg_'
    if pkg.name == 'jagen' then
        prefix = 'jagen_'
    end

    for i = 1, #main do
        main[i] = prefix..main[i]
    end
    for i = 1, #exports do
        exports[i] = 'pkg_export_'..exports[i]
    end

    function prepend_export(list)
        for i = 1, #list do
            list[i] = 'export '..list[i]
        end
    end

    for list in each { main, exports, imports, env } do
        prepend_export(list)
    end

    local file = assert(io.open(filename, 'w+'))
    file:write(table.concat(main, '\n'), '\n')
    -- if next(exports) then
    --     file:write('\n', table.concat(exports, '\n'), '\n')
    -- end
    -- if next(imports) then
    --     file:write('\n', table.concat(imports, '\n'), '\n')
    -- end
    -- if next(unset) then
    --     file:write('\n', table.concat(unset, '\n'), '\n')
    -- end
    if next(env) then
        file:write('\n', table.concat(env, '\n'), '\n')
    end
    file:close()
end

return P
