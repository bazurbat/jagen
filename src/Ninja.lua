
Ninja = {
    space = 4
}

local insert = table.insert

function Ninja:new()
    local o = {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Ninja:indent(level)
    level = level or 0
    local out = {}
    for i = 1, level * self.space do
        insert(out, ' ')
    end
    return table.concat(out)
end

function Ninja:line(list, level)
    local out = {}
    insert(out, list[1])
    if #list > 1 then
        for i = 2, #list do
            insert(out, self:indent(level or 1)..list[i])
        end
    end
    return table.concat(out, ' $\n')
end

function Ninja:variable(key, value, level)
    return string.format('%s%s = %s', self:indent(level), key, value)
end

function Ninja:rule(rule)
    local o = {
        string.format('\nrule %s', rule.name),
        self:variable('command', rule.command, 1)
    }
    if rule.variables then
        local vars = {}
        for k, v in pairs(rule.variables) do
            insert(vars, self:variable(k, v, 1))
        end
        table.sort(vars)
        table.iextend(o, vars)
    end
    return table.concat(o, '\n')
end

function Ninja:header()
    local o = {
        self:variable('builddir', Jagen.build_dir),
        self:rule({
                name    = 'command',
                command = '$command'
            }),
        self:rule({
                name    = 'script',
                command = '$script && touch $out'
            }),
    }
    return table.concat(o)
end

function Ninja:format_stage(target)
    local o = {}
    
    local function format_outputs()
        local o = { tostring(target) }
        if target.outputs then
            table.sort(target.outputs)
            table.iextend(o, target.outputs)
        end
        if #o > 1 then
            insert(o, self:indent(1))
        end
        return self:line(o, 2)
    end

    local function format_inputs()
        local o = table.imap(target.inputs or {}, tostring)
        table.sort(o)
        insert(o, 1, '')
        return self:line(o, 4)
    end

    local function format_script()
        local o = {}
        if Jagen.shell and #Jagen.shell > 0 then
            insert(o, Jagen.shell)
        end
        insert(o, 'jagen-pkg')
        insert(o, assert(target.name))
        insert(o, assert(target.stage))
        insert(o, target.config or "''")
        if target.arg then
            insert(o, "'"..string.gsub(target.arg, '%$', '$$').."'")
        end
        return table.concat(o, ' ')
    end

    insert(o, string.format('\nbuild %s: script%s',
            format_outputs(), format_inputs()))

    insert(o, self:variable('description', target:__tostring(' '), 1))
    insert(o, self:variable('script', format_script(), 1))

    return table.concat(o, '\n')
end

function Ninja:format_package(pkg)
    local out = {}
    for stage in pkg:each() do
        insert(out, self:format_stage(stage))
    end
    return table.concat(out)
end

function Ninja:generate(out_file, rules)
    local out = io.open(out_file, 'w')
    local packages = {}

    for _, rule in pairs(rules) do
        insert(packages, rule)
    end

    table.sort(packages, function (a, b)
            return a.name < b.name
        end)

    out:write(self:header())
    out:write('\n')
    for _, pkg in ipairs(packages) do
        out:write(self:format_package(pkg))
        out:write('\n')
    end

    out:close()
end
