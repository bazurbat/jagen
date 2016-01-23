local system = require 'system'
local rules  = require 'rules'

local function exists(pathname)
    assert(type(pathname) == 'string')
    return os.execute(string.format('test -e "%s"', pathname)) == 0
end

local P = {}

function P:new(o)
    local o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function P:packages(names)
    local packages = rules.merge(rules.load())
    local scm_packages = {}

    if names and #names > 0 then
        for _, name in ipairs(names) do
            if not packages[name] then
                jagen.die('no such package: %s', name)
            end
            if not packages[name].source:is_scm() then
                jagen.die('not scm package: %s', name)
            end
            table.insert(scm_packages, packages[name])
        end
    else
        for _, pkg in pairs(packages) do
            if pkg.source:is_scm() then
                table.insert(scm_packages, pkg)
            end
        end
    end

    table.sort(scm_packages, function (a, b)
            return a.name < b.name
        end)

    return scm_packages
end

-- Should return 0 if true, 1 if false, for shell scripting.
function P:dirty_command(names)
    for _, pkg in ipairs(self:packages(names)) do
        if pkg.source:dirty() then
            return 0
        end
    end
    return 1
end

function P:status_command(names)
    for _, pkg in ipairs(self:packages(names)) do
        local source = pkg.source
        if exists(source.path) then
            local dirty = source:dirty() and 'dirty' or ''
            local head = source:head()
            if not head then
                jagen.die('failed to get source head for %s in %s',
                    pkg.name, source.path)
            end
            print(string.format("%s (%s): %s %s", pkg.name, source.location, head, dirty))
        else
            print(string.format("%s (%s): not exists", pkg.name, source.location))
        end
    end
end

function P:clean_command(names)
    for _, pkg in ipairs(self:packages(names)) do
        if not pkg.source:clean() then
            jagen.die('failed to clean %s (%s) in %s',
                pkg.name, pkg.source.branch, pkg.source.path)
        end
    end
end

function P:update_command(names)
    for _, pkg in ipairs(self:packages(names)) do
        if not pkg.source:update() then
            jagen.die('failed to update %s to the latest %s in %s',
                pkg.name, pkg.source.branch, pkg.source.path)
        end
    end
end

function P:clone_command(names)
    for _, pkg in ipairs(self:packages(names)) do
        if not pkg.source:clone() then
            jagen.die('failed to clone %s from %s to %s',
                pkg.name, pkg.source.location, pkg.source.path)
        end
    end
end

function P:delete_command(names)
    for _, pkg in ipairs(self:packages(names)) do
        if exists(pkg.source.path) then
            if not system.exec('rm', '-rf', pkg.source.path) then
                jagen.die('failed to delete %s source directory %s',
                    pkg.name, pkg.source.path)
            end
        end
    end
end

return P
