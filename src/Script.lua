local System = require 'System'
local Target = require 'Target'
local Chunk  = require 'Chunk'

local P = {}

local function is_simple_value(value)
    local t = type(value)
    return t == 'string' or t == 'number' or t == 'boolean'
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
        local array = {}
        for i = 1, #value do
            local item = value[i]
            if is_simple_value(item) then
                append(array, tostring(item))
            end
        end
        if #array > 0 then
            fmt("%s='%s'", name, table.concat(array, '\t'))
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

function P:write(pkg, filename)
    local skip = { stages = true, uses = true, import = true, env = true }

    local properties = {}
    for key, val in pairs(pkg) do
        if type(val) == 'string' then
            append(properties, key)
            skip[key] = true
        end
    end
    table.sort(properties)

    local standard_sections = {
        source = true, build = true, install = true, export = true,
        'source', 'build', 'install', 'export'
    }
    local other_sections = {}

    for key, val in pairs(pkg) do
        if not skip[key] and not standard_sections[key] then
            append(other_sections, key)
        end
    end
    table.sort(other_sections)

    local lines, export_lines = {}, {}

    local function add_line(s, ...)
        append(lines, string.format(s, ...))
    end
    local function add_export_line(s, ...)
        append(export_lines, string.format(s, ...))
    end

    for key in each(properties) do
        write(add_line, key, pkg[key])
    end

    for name in each(standard_sections) do
        if pkg[name] ~= nil then
            write(add_line, name, pkg[name])
        end
    end

    if pkg.import ~= nil then
        write(add_line, 'import', pkg.import)
    end

    for name in each(other_sections) do
        write(add_line, name, pkg[name])
    end

    if pkg.env ~= nil then
        write(add_line, 'pkg_env', pkg.env)
        write(add_export_line, '', pkg.env)
    end

    for i = 1, #lines do
        lines[i] = string.format('pkg_%s', lines[i])
    end
    for i = 1, #export_lines do
        export_lines[i] = string.format('export %s', export_lines[i])
    end

    local file = assert(io.open(filename, 'w+'))
    file:write(table.concat(lines, '\n'), '\n')
    if next(export_lines) then
        file:write('\n', table.concat(export_lines, '\n'), '\n')
    end
    file:close()
end

return P
