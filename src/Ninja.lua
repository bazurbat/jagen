
Ninja = {
    space = 4
}

function Ninja:new()
    local o = {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Ninja:indent(level)
    level = level or 0
    local t = {}
    for i = 1, level * self.space do
        table.insert(t, ' ')
    end
    return table.concat(t)
end

function Ninja:variable(k, v, level)
    return string.format('%s%s = %s', self:indent(level), k, v)
end

function Ninja:rule(rule)
    local o = {
        string.format('\nrule %s', rule.name),
        self:variable('command', rule.command, 1)
    }
    if rule.variables then
        local vars = {}
        for k, v in pairs(rule.variables) do
            table.insert(vars, self:variable(k, v, 1))
        end
        table.sort(vars)
        table.iextend(o, vars)
    end
    return table.concat(o, '\n')
end

function Ninja:build(build)
    local header = {
        string.format('\nbuild %s: %s',
            table.concat(build.outputs, ' '), build.rule),
        unpack(map(tostring, build.inputs))
    }
    local o = {
        table.concat(header, ' $\n'..self:indent(4))
    }
    if build.variables then
        local vars = {}
        for k, v in pairs(build.variables) do
            table.insert(vars, self:variable(k, v, 1))
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

function Ninja:build_stage(target)
    local script = {}
    local shell = Jagen.shell
    if shell and #shell > 0 then
        table.insert(script, shell)
    end
    table.insert(script, 'jagen-pkg')
    table.insert(script, assert(target.name))
    table.insert(script, assert(target.stage))
    table.insert(script, target.config or "''")
    if target.arg then
        table.insert(script, "'"..string.gsub(target.arg, '%$', '$$').."'")
    end

    return self:build({
            rule      = 'script',
            outputs   = { tostring(target) },
            inputs    = target.inputs,
            variables = {
                script      = table.concat(script, ' '),
                description = target:__tostring(' ')
            }
        })
end

function Ninja:build_package(pkg)
    local o = {}
    for stage in pkg:each() do
        table.insert(o, self:build_stage(stage))
    end
    return table.concat(o)
end

function Ninja:generate(out_file, rules)
    local out = io.open(out_file, 'w')
    local packages = {}

    for _, rule in pairs(rules) do
        table.insert(packages, rule)
    end

    table.sort(packages, function (a, b)
            return a.name < b.name
        end)

    for _, pkg in ipairs(packages) do
        for stage in pkg:each() do
            table.sort(stage.inputs or {}, function (a, b)
                    return tostring(a) < tostring(b)
                end)
        end
    end


    out:write(self:header())
    out:write('\n')
    for _, pkg in ipairs(packages) do
        out:write(self:build_package(pkg))
        out:write('\n')
    end

    out:close()
end
