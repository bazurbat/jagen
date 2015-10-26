-- require 'pl'

--{{{ common

function copy(t)
    local c = {}
    for k, v in pairs(t) do
        if type(v) == 'table' then
            v = copy(v)
        end
        c[k] = v
    end
    return c
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
    for i, v in ipairs(list or {}) do
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
    if jagen.output_file then
        table.insert(command, string.format('| tee -a %s', jagen.output_file))
    end
    local line = table.concat(command, ' ')
    jagen.debug1(line)
    local status = os.execute(line)
    return status % 0xFF
end

--}}}
--{{{ Rule

Rule = {
    default_stages = { 'clean', 'unpack', 'patch' }
}
Rule.__index = Rule

function Rule:new(o)
    o = o or {}
    setmetatable(o, self)
    return o
end

function Rule:from_rules(...)
    local rules = Rule:new()
    for _, arg in ipairs({...}) do
        table.merge(rules, arg)
    end
    rules:convert_name()

    local pkg = Rule:from_file(rules.name)
    table.merge(pkg, rules)
    pkg:convert_source()

    for _, stage in ipairs(self.default_stages) do
        pkg:add_target(Target.new(pkg.name, stage))
    end

    pkg:add_build_dependencies()

    for _, stage in ipairs(pkg) do
        local target = Target.from_rule(pkg, stage)
        target.config = pkg.config
        pkg:add_target(target)
    end

    return pkg
end

function Rule:from_file(name)
    local path = system.mkpath(jagen.pkg_dir, name..'.lua')
    local env = {}
    local o

    function env.package(rule)
        o = rule
    end

    local def = loadfile(path)
    if def then
        setfenv(def, env)
        def()
    end

    return Rule:new(o)
end

function Rule:add_target(target)
    self.stages = self.stages or {}
    local function equal(t) return t == target end
    local existing = find(equal, self.stages)
    if existing then
        existing:append(target)
    else
        table.insert(self.stages, target)
    end
    return self
end

function Rule:merge(rule)
    for k, v in pairs(rule) do
        if type(k) ~= 'number' and k ~= 'stages' then
            if type(v) == 'table' then
                self[k] = table.merge(self[k] or {}, v)
            else
                self[k] = v
            end
        end
    end
    for _, s in ipairs(rule.stages or {}) do
        self:add_target(s)
    end
    for _, v in ipairs(rule) do
        table.insert(self, v)
    end
    return self
end

function Rule:convert_name()
    local function is_string(v)
        return type(v) == 'string'
    end
    local i
    self.name, i = find(is_string, self)
    if i then
        table.remove(self, i)
        local config, i = find(is_string, self)
        if i then
            self.config = config
            table.remove(self, i)
        end
    end
end

function Rule:convert_source()
    local source = self.source
    if type(source) == 'string' then
        self.source = { type = 'dist', location = source }
    end
    return self
end

function Rule:add_toolchain_dependency()
    local function is_build_stage(target)
        return target.stage == 'build'
    end
    for _, stage in ipairs(filter(is_build_stage, self.stages)) do
        table.insert(stage.inputs, 1, Target.new('toolchain'))
    end
end

function Rule:add_build_dependencies()
    local build = self.build
    if build then
        if build.with_provided_libtool then
            local target = Target.new(self.name, 'configure')
            target.inputs = { Target.new('libtool', 'install', 'host') }
            self:add_target(target)
        end
        if build.type then
            self:add_target(Target.new(self.name, 'build', self.config))
            self:add_target(Target.new(self.name, 'install', self.config))
        end
    end
end

function Rule:add_ordering_dependencies()
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

function Rule:type()
    local source = self.source
    return source and source.type
end

function Rule:is_source()
    local source_type = Rule.type(self)
    return source_type == 'git' or source_type == 'hg'
end

function Rule:directory()
    local function basename(location)
        return location and io.popen('basename '..location..' .git'):read()
    end
    if Rule.is_source(self) then
        local location = self.source.location
        local name = self.source.directory or basename(location)
        return system.mkpath(jagen.src_dir, name or self.name)
    else
        return system.mkpath(jagen.build_dir, 'pkg', self.name)
    end
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

function Ninja:build_toolchain()
    return self:build({
            rule      = 'command',
            outputs   = { 'toolchain' },
            variables = { command = jagen.cmd..' toolchain' }
        })
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
    out:write(self:build_toolchain())
    out:write('\n')
    for _, pkg in ipairs(packages) do
        out:write(self:build_package(pkg))
        out:write('\n')
    end

    out:close()
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
        needs  = {},
        inputs = {}
    }
    setmetatable(target, Target)
    return target
end

function Target.from_list(list)
    return Target.new(list[1], list[2], list[3])
end

function Target.from_rule(pkg, rule, config)
    local stage = rule[1]; assert(type(stage) == 'string')
    local target = Target.new(pkg.name, stage, config or pkg.config)

    for i = 2, #rule do
        table.insert(target.inputs, Target.from_list(rule[i]))
    end

    for _, name in ipairs(rule.needs or {}) do
        target.needs[name] = true
        table.insert(target.inputs, Target.new(name, 'install', pkg.config))
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

function Target:append(target)
    for name, _ in pairs(target.needs or {}) do
        self.needs[name] = true
    end

    for _, i in ipairs(target.inputs or {}) do
        local k = tostring(i)
        if not self.inputs[k] then
            self.inputs[k] = true
            table.insert(self.inputs, i)
        end
    end

    return self
end

--}}}
--{{{ jagen

jagen =
{
    shell = os.getenv('pkg_shell'),

    debug = os.getenv('pkg_debug'),
    flags = os.getenv('pkg_flags'),
    sdk   = os.getenv('pkg_sdk'),

    bin_dir   = os.getenv('pkg_bin_dir'),
    lib_dir   = os.getenv('pkg_lib_dir'),
    src_dir   = os.getenv('pkg_src_dir'),
    build_dir = os.getenv('pkg_build_dir'),

    patch_dir         = os.getenv('pkg_patch_dir'),
    build_include_dir = os.getenv('pkg_build_include_dir'),
    private_dir       = os.getenv('pkg_private_dir'),

    output = nil,
    output_file = nil,
}

jagen.pkg_dir = system.mkpath(jagen.lib_dir, 'pkg')

jagen.cmd = system.mkpath(jagen.lib_dir, 'cmd.sh')
jagen.rules_file = system.mkpath(jagen.lib_dir, 'rules.'..jagen.sdk..'.lua')
jagen.build_file = system.mkpath(jagen.build_dir, 'build.ninja')

function jagen.exec(...)
    return system.exec(jagen.cmd, ...)
end

function jagen.write(...)
    if jagen.output then
        local o = jagen.output
        o:seek('end')
        o:write(concat(...), '\n')
        o:flush()
    end
end

function jagen.message(...)
    jagen.write(...)
    print(string.format('\027[1;34m:::\027[0m %s', concat(...)))
end
function jagen.warning(...)
    jagen.write('Warning: ', ...)
    print(string.format('\027[1;33m:::\027[0m %s', concat(...)))
end
function jagen.error(...)
    jagen.write('Error: ', ...)
    print(string.format('\027[1;31m:::\027[0m %s', concat(...)))
end

function jagen.debug(...)
    if jagen.debug then
        jagen.write('D: ', ...)
        print(string.format('\027[1;36m:::\027[0m %s', concat(...)))
    end
end
function jagen.debug1(...)
    if os.getenv('pkg_debug') >= '1' then
        jagen.write('D1: ', ...)
        print(string.format('\027[1;36m:::\027[0m %s', concat(...)))
    end
end
function jagen.debug2(...)
    if os.getenv('pkg_debug') >= '2' then
        jagen.write('D2: ', ...)
        print(string.format('\027[1;36m:::\027[0m %s', concat(...)))
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

function jagen.load_rules()
    local packages = {}
    local env = {
        jagen = jagen
    }

    function env.package(...)
        local pkg  = Rule:from_rules(...)
        packages[pkg.name] = pkg
        table.insert(packages, pkg)
    end

    local rules = loadfile(jagen.rules_file)
    if rules then
        setfenv(rules, env)
        rules()
    end

    local function add_unresolved(pkg)
        local added = {}
        for _, stage in ipairs(pkg.stages) do
            for name, _ in pairs(stage.needs) do
                if packages[name] then
                    local p = packages[name]
                    p:add_target(Target.new(name, 'build', pkg.config))
                    p:add_target(Target.new(name, 'install', pkg.config))
                    for _, rule in ipairs(pkg.inject or {}) do
                        local t = Target.from_rule(p, rule, pkg.config)
                        p:add_target(t)
                    end
                else
                    local p = Rule:from_rules { name, pkg.config }
                    p.inject = pkg.inject or {}
                    for _, rule in ipairs(pkg.inject or {}) do
                        local t = Target.from_rule(p, rule)
                        p:add_target(t)
                    end
                    packages[name] = p
                    table.insert(packages, p)
                end
            end
        end
        return added
    end

    for _, pkg in ipairs(packages) do
        local added = {}
        repeat
            added = add_unresolved(pkg)
        until #added == 0
    end

    rules = {}

    for _, pkg in ipairs(packages) do
        local name = pkg.name
        local rule = rules[name]
        if rule then
            rule:merge(pkg)
        else
            rules[name] = pkg
            table.insert(rules, pkg)
        end
    end

    for _, pkg in ipairs(rules) do
        pkg:add_toolchain_dependency()
        pkg:add_ordering_dependencies()
    end

    return rules
end

function jagen.generate_include_script(pkg)
    local name     = pkg.name
    local filename = name..'.sh'
    local path     = system.mkpath(jagen.build_include_dir, filename)
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
    if self.pkg.build then
        table.insert(script, self:build())
    end
    if self.pkg.source then
        table.insert(script, self:source())
    end
    if self.pkg.patches then
        table.insert(script, self:patch())
    end

    return table.concat(script, '\n\n')
end

function Script:header()
    return '#!/bin/sh'
end

function Script:build()
    local build = self.pkg.build
    local o = {}
    if build.type == 'GNU' then
        if build.options then
            table.insert(o, string.format('p_options=\'%s\'', build.options))
        end
        if build.libs then
            table.insert(o, string.format("p_libs='%s'",
                table.concat(build.libs, ' ')))
        end
        if build.with_provided_libtool then
            table.insert(o, 'p_with_provided_libtool="yes"')
        end
    end
    if build.dir then
        table.insert(o, 'p_build_dir="'..build.dir..'"')
    end

    return table.concat(o, '\n')
end

function Script:source()
    local pkg = self.pkg
    local source = pkg.source
    local o, s = {}, {}
    if source.type == 'git' or source.type == 'hg' then
        table.insert(s, source.type)
        table.insert(s, source.location)
    elseif source.type == 'dist' then
        table.insert(s, system.mkpath('$pkg_dist_dir', source.location))
    end
    table.insert(o, string.format('p_source="%s"', table.concat(s, ' ')))
    if source.branch then
        table.insert(o, string.format('p_source_branch="%s"', source.branch))
    end
    if pkg:is_source() then
        table.insert(o, string.format('p_source_dir="%s"', self.pkg:directory()))
    end
    return table.concat(o, '\n')
end

function Script:patch()
    local o = {}
    table.insert(o, 'pkg_patch_pre() {')
    for _, patch in ipairs(self.pkg.patches or {}) do
        local name = patch[1]
        local strip = patch[2]
        table.insert(o, string.format('  p_patch %d "%s"', strip, name))
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
--{{{ Source

Source = {}

function Source:new(p)
    local o = { package = p }
    setmetatable(o, self)
    self.__index = self
    return o
end

function Source:create(p)
    local kind = p:type()
    if kind == 'git' then
        return GitSource:new(p)
    elseif kind == 'hg' then
        return HgSource:new(p)
    else
        return Source:new(p)
    end
end

function Source.name(filename)
    local name = string.match(filename, '^.*/(.+)') or filename
    local function m(ext)
        return string.match(name, '^([%w_.-]+)'..ext)
    end
    return m('%.tar') or m('%.tgz') or m('%.tbz2') or m('%.txz') or m('%.zip')
end

GitSource = Source:new()

function GitSource:exec(...)
    local dir = self.package:directory()
    return system.exec('git', '-C', dir, ...)
end

function GitSource:popen(...)
    local dir = self.package:directory()
    return io.popen('git -C '..dir..' '..concat(...)):read() or ''
end

function GitSource:head()
    return self:popen('rev-parse', 'HEAD')
end

function GitSource:dirty()
    return string.len(self:popen('status', '--porcelain')) > 0
end

function GitSource:fetch()
    local status = 0
    status = self:exec('fetch', '--prune', '--no-tags')
    if status > 0 then
        jagen.die("failed to fetch "..self.package.name)
    end
end

function GitSource:checkout(branch)
    local status = 0
    branch = branch or 'master'
    local exist = self:popen('branch', '--list', branch)
    if #exist > 0 then
        if string.sub(exist, 1, 1) ~= '*' then
            status = self:exec('checkout', branch)
        end
    else
        status = self:exec('checkout',
            '-b', branch, '-t', 'origin/'..branch)
    end
    if status > 0 then
        jagen.die('failed to checkout '..branch..' branch of '..self.package.name)
    end
end

function GitSource:pull()
    local status = 0
    status = self:exec('pull', '--ff-only')
    if status > 0 then
        jagen.die('failed to pull '..self.package.name)
    end
end

HgSource = Source:new()

function HgSource:exec(...)
    local dir = self.package:directory()
    return system.exec('hg', '-R', dir, ...)
end

function HgSource:popen(...)
    local dir = self.package:directory()
    return io.popen('hg -R '..dir..' '..concat(...)):read() or ''
end

function HgSource:head()
    return self:popen('id', '-i')
end

function HgSource:dirty()
    return string.len(self:popen('status')) > 0
end

function HgSource:fetch()
    local status = 0
    status = self:exec('pull')
    if status > 0 then
        jagen.die("Failed to fetch "..self.package.name)
    end
end

function HgSource:checkout(branch)
    local status = 0
    status = self:exec('update', '-c')
    if status > 0 then
        jagen.die('failed to checkout '..self.package.name)
    end
end

function HgSource:pull()
    local status = 0
    status = self:exec('pull', '-u')
    if status > 0 then
        jagen.die('failed to pull '..self.package.name)
    end
end

--}}}
--{{{ src

src = {}

function src.name(filename)
    print(Source.name(filename))
end

function src.status(args)
    local packages = jagen.load_rules()
    local source_packages = filter(Rule.is_source, packages)

    for _, p in ipairs(source_packages) do
        local s = Source:create(p)
        local dirty = s:dirty() and 'dirty' or ''
        local head = s:head()
        print(string.format("%s: %s %s", p.name, head, dirty))
    end
end

function src.update(...)
    jagen.output_file = system.mkpath(jagen.build_dir, 'update.log')
    jagen.output = assert(io.open(jagen.output_file, 'w'))

    local packages = jagen.load_rules()
    local source_packages = filter(Rule.is_source, packages)

    for _, p in ipairs(source_packages) do
        jagen.message("update "..p.name)
        local source = Source:create(p)
        source:fetch()
        source:checkout(p.source.branch)
        source:pull()
    end

    if jagen.output then
        jagen.output:close()
    end
end

--}}}

command = arg[1]
status = 0

if command == 'refresh' then
    jagen.generate()
elseif command == 'build' then
    local args = table.rest(arg, 2)

    status = jagen.build(args)
elseif command == 'rebuild' then
    local args = table.rest(arg, 2)

    status = jagen.rebuild(args)
elseif command == 'src' then
    local subcommand = arg[2]
    local args = table.rest(arg, 3)

    if not subcommand then
        jagen.message('Available src subcommands: name, status, update')
    elseif subcommand == 'name' then
        src.name(unpack(args))
    elseif subcommand == 'status' then
        src.status(args)
    elseif subcommand == 'update' then
        src.update(args)
    else
        jagen.die('Unknown src subcommand:', subcommand);
    end
else
    jagen.die('Unknown command:', command)
end

os.exit(status % 0xFF)
