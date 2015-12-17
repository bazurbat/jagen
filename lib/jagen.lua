-- require 'pl'

--{{{ common

function copy(o)
    if type(o) == 'table' then
        local c = {}
        for k, v in pairs(o) do
            c[k] = copy(v)
        end
        return c
    else
        return o
    end
end

function append(...)
    local o = {}
    for _, arg in ipairs({...}) do
        for _, i in ipairs(arg) do
            table.insert(o, i)
        end
    end
    return o
end

function map(f, t)
    local r = {}
    for i, v in ipairs(t or {}) do
        table.insert(r, f(v))
    end
    return r
end

function filter(pred, list)
    local o = {}
    for _, v in ipairs(list or {}) do
        if pred(v) then
            table.insert(o, v)
        end
    end
    return o
end

function find(pred, list)
    for i, v in ipairs(list) do
        if pred(v) then
            return v, i
        end
    end
    return nil, nil
end

function concat(...)
    return table.concat(map(tostring, {...}), ' ')
end

function string.split(s, sep)
    local o, b, e = {}
    local init = 1

    repeat
        b, e = string.find(s, sep, init, true)
        if not b then b = 0 end
        table.insert(o, string.sub(s, init, b-1))
        if e then init = e + 1 end
    until b == 0

    return o
end

function table.rest(t, start)
    local o = {}
    for i = start, #t do
        table.insert(o, t[i])
    end
    return o
end

function table.merge(a, b)
    for k, v in pairs(b) do
        if type(k) ~= 'number' then
            if type(v) == 'table' then
                a[k] = table.merge(a[k] or {}, v)
            else
                a[k] = v
            end
        end
    end
    for _, v in ipairs(b) do
        table.insert(a, v)
    end
    return a
end

function table.dump(t, i)
    local i = i or 0
    if type(t) ~= 'table' then
        io.write(tostring(t), '\n')
        return
    end
    io.write(string.rep(' ', i), tostring(t), ' {\n')
    for k, v in pairs(t) do
        io.write(string.rep(' ', i+2), k, ' = ')
        if type(v) == 'table' then
            io.write('\n')
            table.dump(v, i+4)
        else
            io.write(tostring(v), '\n')
        end
    end
    io.write(string.rep(' ', i), '}\n')
end

--}}}
--{{{ system

local system = {}

function system.mkpath(...)
    local sep = '/'
    local path = {}
    for _, c in ipairs({...}) do
        table.insert(path, c)
    end
    return table.concat(path, sep)
end

function system.exec(...)
    local command = {}
    for _, arg in ipairs({...}) do
        table.insert(command, string.format('%q', tostring(arg)))
    end
    local line = table.concat(command, ' ')
    jagen.debug1(line)
    local status = os.execute(line)
    return status ~= 0, status % 0xFF
end

function system.exists(pathname)
    return not system.exec('test', '-e', pathname)
end

--}}}
--{{{ Target

Target = {}
Target.__index = Target

function Target.new(name, stage, config)
    local target = {
        name   = name,
        stage  = stage,
        config = config,
        inputs = {}
    }
    setmetatable(target, Target)
    return target
end

function Target.from_list(list)
    return Target.new(list[1], list[2], list[3])
end

function Target:parse(rule, name, config)
    local stage = rule[1]; assert(type(stage) == 'string')
    local target = Target.new(name, stage, config)

    for i = 2, #rule do
        table.insert(target.inputs, Target.from_list(rule[i]))
    end

    return target
end

function Target.from_arg(arg)
    local name, stage, config
    local c = string.split(arg, ':')

    if c[1] and #c[1] > 0 then
        name = c[1]
    end
    if c[2] and #c[2] > 0 then
        stage = c[2]
    end
    if c[3] and #c[3] > 0 then
        config = c[3]
    end

    return Target.new(name, stage, config)
end

function Target.__eq(a, b)
    return a.name == b.name and
    a.stage == b.stage and
    a.config == b.config
end

function Target.__tostring(t, sep)
    local o = {}
    sep = sep or '-'
    if t.name then table.insert(o, t.name) end
    if t.stage then table.insert(o, t.stage) end
    if t.config then table.insert(o, t.config) end
    return table.concat(o, sep)
end

function Target:add_inputs(target)
    for _, i in ipairs(target.inputs or {}) do
        local function eq(t)
            return t == i
        end
        local found = find(eq, self.inputs)
        if not found then
            table.insert(self.inputs, i)
        end
    end

    return self
end

--}}}
--{{{ Package

Package = {
    init_stages = { 'clean', 'unpack', 'patch' }
}
Package.__index = Package

function Package:__tostring()
    local o = {}
    if self.name then table.insert(o, self.name) end
    if self.config then table.insert(o, self.config) end
    return table.concat(o, ':')
end

function Package:add(rule, packages)
    local pkg, name = {}

    if type(rule[1]) == 'string' then
        rule.name = rule[1]
        table.remove(rule, 1)
    end
    if type(rule[1]) == 'string' then
        rule.config = rule[1]
        table.remove(rule, 1)
    end

    jagen.debug2('add', rule.name, rule.config)

    name = rule.name
    if packages[name] then
        pkg = packages[name]
        pkg:parse(rule)
    else
        pkg = Package:new(rule)
        packages[name] = pkg
        table.insert(packages, pkg)
    end

    if type(pkg.source) == 'string' then
        pkg.source = { type = 'dist', location = pkg.source }
    end
end

function Package:load(filename)
    local o, env = {}, {}
    function env.package(rule)
        o = rule
    end
    local chunk = loadfile(filename)
    if chunk then
        setfenv(chunk, env)
        chunk()
    end
    return o
end

function Package:parse(rule)
    table.merge(self, rule)
    self:add_build_targets(rule.config)
    for _, stage in ipairs(rule) do
        self:add_target(Target:parse(stage, self.name, rule.config))
    end
end

function Package:new(rule)
    local pkg = { stages = {} }
    setmetatable(pkg, self)

    for _, name in ipairs(self.init_stages) do
        pkg:add_target(Target.new(rule.name, name))
    end

    local pathname = system.mkpath('pkg', rule.name..'.lua')
    for _, filename in ipairs(jagen.import_paths(pathname)) do
        pkg:parse(table.merge(Package:load(filename), rule))
    end

    return pkg
end

function Package:add_target(target)
    local function default(this)
        for _, stage in ipairs(self.init_stages) do
            if stage == target.stage and stage == this.stage then
                return this
            end
        end
    end
    local function eq(this)
        return this == target or default(this)
    end

    local found = find(eq, self.stages)
    if found then
        jagen.debug2(tostring(self), '=', tostring(target))
        found:add_inputs(target)
        return self
    else
        jagen.debug2(tostring(self), '+', tostring(target))
        table.insert(self.stages, target)
    end

    return self
end

function Package:add_build_targets(config)
    jagen.debug2('add_build_targets', self, config)
    local build = self.build
    if build then
        if build.with_provided_libtool then
            local target = Target.new(self.name, 'configure')
            target.inputs = { Target.new('libtool', 'install', 'host') }
            self:add_target(target)
        end
        if build.type ~= 'manual' then
            local build_rule = { 'build' }
            if config == 'target' then
                build_rule = { 'build', { 'toolchain', 'install', 'target' } }
            end
            self:add_target(Target:parse(build_rule, self.name, config))
            self:add_target(Target.new(self.name, 'install', config))
        end
    end
end

function Package:add_ordering_dependencies()
    local prev, common

    for _, s in ipairs(self.stages) do
        if prev then
            if common and s.config ~= prev.config then
                table.insert(s.inputs, 1, common)
            else
                table.insert(s.inputs, 1, prev)
            end
        end

        prev = s
        if not s.config then
            common = s
        end
    end
end

function Package:is_source()
    local source = self.source
    if source then
        return source.type == 'git' or source.type == 'hg'
    else
        return false
    end
end

function Package:directory()
    local location  = self.source.location
    local directory = self.source.directory
    local function basename(location)
        return location and io.popen('basename '..location..' .git'):read()
    end
    if self:is_source() then
        directory = directory or basename(location) or self.name
        return system.mkpath('$jagen_src_dir', directory)
    else
        directory = directory or Source.name(location) or self.name
        return system.mkpath('$pkg_work_dir', directory)
    end
end

--}}}
--{{{ Source

Source = {}

function Source.name(filename)
    local name = string.match(filename, '^.*/(.+)') or filename
    local function m(ext)
        return string.match(name, '^([%w_.-]+)'..ext)
    end
    return m('%.tar') or m('%.tgz') or m('%.tbz2') or m('%.txz') or m('%.zip')
end

function Source:create(o)
    local source = {}

    if o then
        if o.type == 'git' then
            source = GitSource:new(o)
        elseif o.type == 'hg' then
            source = HgSource:new(o)
        end
    else
        source = Source:new(o)
    end

    local function basename(location)
        return location and io.popen('basename '..location..' .git'):read()
    end

    if source.location then
        if source.type == 'git' or source.type == 'hg' then
            source.directory = system.mkpath('$jagen_src_dir',
                source.directory or basename(source.location))
        else
            source.directory = system.mkpath('$pkg_work_dir',
                source.directory or Source.name(source.location))
        end
    end

    return source
end

function Source:new(o)
    local o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

GitSource = Source:new()

function GitSource:exec(...)
    assert(self.directory)
    return system.exec('git', '-C', self.directory, ...)
end

function GitSource:popen(...)
    assert(self.directory)
    return io.popen('git -C '..self.directory..' '..concat(...)):read()
end

function GitSource:head()
    return self:popen('rev-parse', 'HEAD')
end

function GitSource:dirty()
    return self:popen('status', '--porcelain')
end

function GitSource:branch_list(branch)
    assert(branch)
    return self:popen('branch', '--list', branch)
end

function GitSource:branch_active(name)
    assert(name)
    return string.sub(name, 1, 1) == '*'
end

function GitSource:branch_remote_add(branch)
    assert(branch)
    return self:exec('remote', 'set-branches', '--add', 'origin', branch)
end

function GitSource:fetch(branch)
    assert(branch)
    local args = { 'fetch', '--prune', '--no-tags', 'origin' }
    if branch then
        local src = 'refs/heads/'..branch
        local dst = 'refs/remotes/origin/'..branch
        table.insert(args, '+'..src..':'..dst)
    end
    return self:exec(unpack(args))
end

function GitSource:checkout(branch)
    assert(branch)
    local name = self:branch_list(branch)
    if #name > 0 then
        if self:branch_active(name) then
            return true
        else
            return self:exec('checkout', branch)
        end
    else
        return self:branch_remote_add(branch) and
            self:exec('checkout', '-b', branch, '-t', 'origin/'..branch)
    end
end

function GitSource:merge(branch)
    assert(branch)
    return self:exec('merge', '--ff-only', 'origin/'..branch)
end

function GitSource:clean()
    return self:exec('checkout', 'HEAD', '.') and self:exec('clean', '-fxd')
end

function GitSource:update()
    local branch = self.package.source.branch or 'master'
    return self:fetch(branch) and self:checkout(branch) and self:merge(branch)
end

function GitSource:clone()
    local command = { 'git', 'clone', '--progress', '--depth', 1 }
    if self.branch then
        table.insert(command, '--branch')
        table.insert(command, self.branch)
    end
    table.insert(command, self.location)
    table.insert(command, self.directory)

    return system.exec(unpack(command))
end

HgSource = Source:new()

function HgSource:exec(...)
    assert(self.directory)
    return system.exec('hg', '-R', self.directory, ...)
end

function HgSource:popen(...)
    assert(self.directory)
    return io.popen('hg -R '..self.directory..' '..concat(...)):read()
end

function HgSource:head()
    return self:popen('id', '-i')
end

function HgSource:dirty()
    return self:popen('status')
end

function HgSource:clean()
    return self:exec('update', '-C') and self:exec('purge', '--all')
end

function HgSource:update()
    local args = { 'update' }
    local branch = self.package.source.branch
    if branch then
        table.insert(args, '-r')
        table.insert(args, branch)
    end
    return self:exec('pull') and self:exec(unpack(args))
end

function HgSource:clone()
    local command = { 'hg', 'clone', '-r', 'tip' }
    table.insert(command, self.package.source.location)
    table.insert(command, self.package:directory())
    return system.exec(unpack(command))
end

--}}}
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
            concat(unpack(build.outputs)), build.rule),
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
--{{{ jagen

jagen =
{
    dir  = os.getenv('jagen_dir'),
    root = os.getenv('jagen_root'),

    overlays = os.getenv('jagen_overlays'),

    shell = os.getenv('jagen_shell'),

    debug = os.getenv('jagen_debug'),
    flags = os.getenv('jagen_flags'),

    lib_dir     = os.getenv('jagen_lib_dir'),
    src_dir     = os.getenv('jagen_src_dir'),
    build_dir   = os.getenv('jagen_build_dir'),
    include_dir = os.getenv('jagen_include_dir'),

    patch_dir   = os.getenv('jagen_patch_dir'),
    private_dir = os.getenv('jagen_private_dir'),
}

jagen.cmd = system.mkpath(jagen.lib_dir, 'cmd.sh')
jagen.build_file = system.mkpath(jagen.build_dir, 'build.ninja')

function jagen.message(...)
    io.write('(I) ', string.format(...), '\n')
    io.flush()
end

function jagen.warning(...)
    io.stderr:write('(W) ', string.format(...), '\n')
    io.stderr:flush()
end

function jagen.error(...)
    io.stderr:write('(E) ', string.format(...), '\n')
    io.stderr:flush()
end

function jagen.debug0(...)
    if jagen.debug then
        io.write('(D) ', string.format(...), '\n')
        io.flush()
    end
end

function jagen.debug1(...)
    if jagen.debug >= '1' then
        io.write('(D) ', string.format(...), '\n')
        io.flush()
    end
end

function jagen.debug2(...)
    if jagen.debug >= '2' then
        io.write('(D) ', string.format(...), '\n')
        io.flush()
    end
end

function jagen.die(...)
    jagen.error(...)
    os.exit(1)
end

function jagen.flag(f)
    for w in string.gmatch(jagen.flags, "[_%w]+") do
        if w == f then
            return true
        end
    end
    return false
end

function jagen.import_paths(filename)
    local o = {}
    table.insert(o, system.mkpath(jagen.dir, 'lib', filename))
    for _, overlay in ipairs(string.split(jagen.overlays, ' ')) do
        table.insert(o, system.mkpath(jagen.dir, 'overlay', overlay, filename))
    end
    table.insert(o, system.mkpath(jagen.root, filename))
    return o
end

function jagen.load_rules()
    local packages = {}

    local function load_rules(filename)
        local rules = {}
        local env = {
            table = table,
            jagen = jagen
        }
        function env.package(rule)
            Package:add(rule, packages)
        end
        local chunk = loadfile(filename)
        if chunk then
            setfenv(chunk, env)
            chunk()
        end
        return rules
    end

    for _, path in ipairs(jagen.import_paths('rules.lua')) do
        load_rules(path)
    end

    Package:add({ 'toolchain', 'target', { 'install' } }, packages)

    for _, pkg in ipairs(packages) do
        pkg:add_ordering_dependencies()
        pkg.source = Source:create(pkg.source)
    end

    return packages
end

function jagen.generate_include_script(pkg)
    local name     = pkg.name
    local filename = name..'.sh'
    local path     = system.mkpath(jagen.include_dir, filename)
    local script   = Script:new(pkg)

    local f = assert(io.open(path, 'w+'))
    f:write(tostring(script))
    f:close()
end

function jagen.generate()
    local packages = jagen.load_rules()
    local ninja = Ninja:new()

    table.sort(packages, function (a, b)
            return a.name < b.name
        end)

    for _, pkg in ipairs(packages) do
        for _, stage in ipairs(pkg.stages) do
            table.sort(stage.inputs, function (a, b)
                    return tostring(a) < tostring(b)
                end)
        end
    end

    ninja:generate(jagen.build_file, packages)

    for _, package in ipairs(packages) do
        jagen.generate_include_script(package)
    end
end

--}}}
--{{{ script

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
    local o, s = {}, {}
    if source.type == 'git' or source.type == 'hg' then
        table.insert(s, source.type)
        table.insert(s, source.location)
    elseif source.type == 'dist' then
        table.insert(s, system.mkpath('$jagen_dist_dir', source.location))
    end
    table.insert(o, string.format('pkg_source="%s"', table.concat(s, ' ')))
    if source.branch then
        table.insert(o, string.format('pkg_source_branch="%s"', source.branch))
    end
    table.insert(o, string.format('pkg_source_dir="%s"', self.pkg:directory()))
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
        if build.with_provided_libtool then
            table.insert(o, 'pkg_with_provided_libtool="yes"')
        end
        if build.in_source then
            build_dir = '$pkg_source_dir'
        end
        if build.directory then
            build_dir = build.directory
        end
    end

    build_dir = build_dir or '$pkg_work_dir/build${pkg_config:+-$pkg_config}'

    table.insert(o, string.format('pkg_build_dir="%s"', build_dir))

    return table.concat(o, '\n')
end

function Script:patch()
    local o = {}
    table.insert(o, 'jagen_pkg_patch_pre() {')
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
--{{{ build

local build = {}

function build.find_targets(packages, arg)
    local targets = {}
    local args = {}

    local function is_param(arg)
        return string.sub(arg, 1, 1) == '-'
    end
    local function match_config(a, b)
        return not a.config or a.config == b.config
    end
    local function match_stage(a, b)
        return not a.stage or a.stage == b.stage
    end
    local function match_target(target, stage)
        return match_stage(target, stage) and match_config(target, stage)
    end

    if is_param(arg) then
        table.insert(args, arg)
    else
        local target = Target.from_arg(arg)
        local packages = target.name and { packages[target.name] } or packages
        for _, pkg in ipairs(packages) do
            for _, stage in ipairs(pkg.stages) do
                if match_target(target, stage) then
                    table.insert(targets, stage)
                end
            end
        end
        if #targets == 0 then
            table.insert(args, arg)
        end
    end

    return targets, args
end

function jagen.build(args)
    local packages = jagen.load_rules()
    local targets = {}

    for _, arg in ipairs(args) do
        targets = append(targets, build.find_targets(packages, arg))
    end

    return system.exec(jagen.cmd, 'build', unpack(targets))
end

function jagen.rebuild(args)
    local packages = jagen.load_rules()
    local targets = {}

    for _, arg in ipairs(args) do
        targets = append(targets, build.find_targets(packages, arg))
    end

    return system.exec(jagen.cmd, 'rebuild', unpack(targets))
end

---}}}
--{{{ src

src = {}

function src.name(filename)
    print(Source.name(filename) or '')
end

function src.packages(names)
    local rules = jagen.load_rules()
    local packages = {}

    if #names > 0 then
        for _, name in ipairs(names) do
            if rules[name] then
                table.insert(packages, rules[name])
            else
                jagen.die('no such package:', name)
            end
        end
    else
        packages = filter(Package.is_source, rules)
    end

    return packages
end

function src.dirty(names)
    for _, pkg in ipairs(src.packages(names)) do
        local source = Source:create(pkg.source)
        if source:dirty() then
            return 1
        end
    end
end

function src.status(names)
    for _, pkg in ipairs(src.packages(names)) do
        if system.exists(pkg.source.directory) then
            local dirty = pkg.source:dirty() and 'dirty' or ''
            local head = pkg.source:head()
            if not head then
                jagen.error('failed to get source head for %s in %s',
                    pkg.name, source.directory)
                return 2
            end
            -- TODO: do actual query instead of using configured location
            print(string.format("%s: %s %s", pkg.source.location, head, dirty))
        else
            print(string.format("%s: not exists", pkg.source.location))
        end
    end
end

function src.clean(names)
    for _, pkg in ipairs(src.packages(names)) do
        if pkg.source:clean() then
            jagen.die('failed to clean %s (%s) in %s',
                pkg.name, pkg.source.branch, pkg.source.directory)
        end
    end
end

function src.update(names)
    for _, pkg in ipairs(src.packages(names)) do
        if pkg.source:update() then
            jagen.die('failed to update %s (%s) in %s',
                pkg.name, pkg.source.branch, pkg.source.directory)
        end
    end
end

function src.clone(names)
    for _, pkg in ipairs(src.packages(names)) do
        if pkg.source:clone() then
            jagen.die('failed to clone %s from %s to %s',
                pkg.name, pkg.source.location, pkg.source.directory)
        end
    end
end

function src.delete(names)
    for _, pkg in ipairs(src.packages(names)) do
        if system.exists(pkg.source.directory) then
            if system.exec('rm', '-rf', pkg.source.directory) then
                jagen.die('failed to delete %s source directory %s',
                    pkg.name, pkg.source.directory)
            end
        end
    end
end

--}}}

command = arg[1]
err = false
status = 0

if command == 'refresh' then
    jagen.generate()
elseif command == 'build' then
    local args = table.rest(arg, 2)

    err, status = jagen.build(args)
elseif command == 'rebuild' then
    local args = table.rest(arg, 2)

    err, status = jagen.rebuild(args)
elseif command == 'src' then
    local subcommand = arg[2]
    local args = table.rest(arg, 3)

    if not subcommand then
        jagen.die('no src subcommand specified')
    elseif src[subcommand] then
        status = src[subcommand](args)
    else
        jagen.die('unknown src subcommand: %s', subcommand)
    end
else
    jagen.die('Unknown command:', command)
end

os.exit((status or 0) % 0xFF)
