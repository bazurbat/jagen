local System = require 'System'
local Target = require 'Target'
local Chunk  = require 'Chunk'

local P = {}

local function write_var(w, name, value)
    local typ = type(value)
    if typ == 'string' or typ == 'number' then
        w("%s='%s'", name, value)
    elseif typ == 'boolean' then
        w("%s='yes'", name, value)
    elseif typ == 'table' then
        if #value > 0 then
            local out = {}
            for i, v in ipairs(value) do
                if type(v) == 'table' then
                else
                    table.insert(out, v)
                end
            end
            w("%s='%s'", name, table.concat(out, '\t'))
        else
            for k, v in pairs(value) do
                write_var(w, name..'_'..k, v)
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

function P:write(pkg, filename)
    local lines = {}

    local skip = { stages = true, import = true, uses = true }
    local values = {}

    local function collect(name, value, result)
        if type(value) == 'table' then
            for key, val in pairs(value) do
                collect(name..'_'..key, val, result)
            end
        else
            values[name] = value
        end
    end

    for key, val in pairs(pkg) do
        if not skip[key] then
            collect(key, val, values)
        end
    end

    collect('import', pkg.import, values)

    print(pretty(values))




    -- write_section(pkg.import)
    --
    -- if pkg.env then
    --     w('')
    --     write_section(pkg.env, 'export ')
    -- end

    local file = assert(io.open(filename, 'w+'))
    file:write(table.concat(lines, '\n'), '\n')
    file:close()
end

return P
