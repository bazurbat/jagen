--{{{ Ninja

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
    return string.format('%s%s = %s\n', self:indent(level), k, v)
end

function Ninja:rule(rule)
    local o = {
        string.format('rule %s', rule.name),
        self:variable('command', rule.command, 1)
    }
    if rule.variables then
        for k, v in pairs(rule.variables) do
            table.insert(o, self:variable(k, v, 1))
        end
    end
    return table.concat(o, '\n')
end

function Ninja:build(build)
    local header = {
        string.format('build %s: %s',
            table.concat(build.outputs, ' '), build.rule),
        unpack(map(tostring, build.inputs))
    }
    local o = {
        table.concat(header, ' $\n'..self:indent(4))
    }
    if build.variables then
        for k, v in pairs(build.variables) do
            table.insert(o, self:variable(k, v, 1))
        end
    end
    return table.concat(o, '\n')
end

function Ninja:header()
    local o = {
        self:variable('builddir', jagen.build_dir),
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
    local shell = jagen.shell
    local script = 'jagen-pkg '..target:__tostring(' ')
    if shell and #shell > 0 then
        script = shell.." "..script
    end
    return self:build({
            rule      = 'script',
            outputs   = { tostring(target) },
            inputs    = target.inputs,
            variables = { script = script }
        })
end

function Ninja:build_package(pkg)
    local o = {}
    for _, stage in ipairs(pkg.stages) do
        table.insert(o, self:build_stage(stage))
    end
    return table.concat(o)
end

function Ninja:generate(out_file, packages)
    local out = io.open(out_file, 'w')

    out:write(self:header())
    out:write('\n')
    for _, pkg in ipairs(packages) do
        out:write(self:build_package(pkg))
        out:write('\n')
    end

    out:close()
end

--}}}
--{{{ Script

Script = {}

function Script:new(pkg)
    local script = { pkg = pkg }
    setmetatable(script, self)
    self.__index = self
    return script
end

function Script:__tostring()
    local script = {
        self:header()
    }
    if self.pkg.source then
        table.insert(script, self:source())
    end
    table.insert(script, self:build())
    if self.pkg.patches then
        table.insert(script, self:patch())
    end

    return table.concat(script, '\n\n')
end

function Script:header()
    return '#!/bin/sh'
end

function Script:source()
    local pkg = self.pkg
    local source = pkg.source
    local o = {}
    if source.type and source.location then
        table.insert(o, string.format(
            'pkg_source="%s %s"', source.type, source.location))
    end
    if source.branch then
        table.insert(o, string.format('pkg_source_branch="%s"', source.branch))
    end
    if source.path then
        table.insert(o, string.format('pkg_source_dir="%s"', source.path))
    end
    return table.concat(o, '\n')
end

function Script:build()
    local o = {}
    local pkg = self.pkg
    local build_dir

    if pkg.build then
        local build = pkg.build

        if build.options then
            table.insert(o, string.format('pkg_options=\'%s\'', build.options))
        end
        if build.libs then
            table.insert(o, string.format("pkg_libs='%s'",
                table.concat(build.libs, ' ')))
        end
        if build.generate then
            table.insert(o, "pkg_build_generate='yes'")
        end
        if build.in_source then
            build_dir = '$pkg_source_dir'
        end
        if build.directory then
            build_dir = build.directory
        end
    end

    build_dir = build_dir or '$pkg_work_dir${pkg_config:+/$pkg_config}'

    table.insert(o, string.format('pkg_build_dir="%s"', build_dir))

    return table.concat(o, '\n')
end

function Script:patch()
    local o = {}
    table.insert(o, 'jagen_pkg_apply_patches() {')
    table.insert(o, '  pkg_run cd "$pkg_source_dir"')
    for _, patch in ipairs(self.pkg.patches or {}) do
        local name = patch[1]
        local strip = patch[2]
        table.insert(o, string.format('  pkg_run_patch %d "%s"', strip, name))
    end
    table.insert(o, '}')
    return table.concat(o, '\n')
end

--}}}
