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

function find(list, pred)
    for _, item in ipairs(list or {}) do
        if pred(item) then
            return item
        end
    end
    return nil
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

function system.exec(command, ...)
    local cmd = { command }
    for _, arg in ipairs({...}) do
        table.insert(cmd, string.format('%q', tostring(arg)))
    end
    local line = table.concat(cmd, ' ')
    jagen.debug1(line)
    local status = os.execute(line)
    return status
end

--}}}
--{{{ Package

Package = {
    default_stages = { { 'clean' }, { 'unpack' }, { 'patch' } }
}

function Package:new(o)
    o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Package:load_rule(rule, stages)
    local pkg = Package.load(rule.name)
    pkg:merge(rule)
    pkg:parse_source()

    pkg.stages = append(copy(self.default_stages), stages or {}, pkg)
    pkg:add_special_dependencies()

    return pkg
end

function Package.load(name)
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

    return Package:new(o)
end

function Package:merge(rule)
    for k, v in pairs(rule) do
        if type(k) ~= 'number' then
            if type(v) == 'table' then
                self[k] = Package.merge(self[k] or {}, v)
            else
                self[k] = v
            end
        end
    end
    for _, v in ipairs(rule) do
        table.insert(self, v)
    end
    return self
end

function Package:parse_source()
    local source = self.source
    if type(source) == 'string' then
        self.source = { type = 'dist', location = source }
    end
    return self
end

function Package:add_special_dependencies()
    local build = self.build
    if build and build.need_libtool then
        table.insert(self.stages, { 'patch', { 'libtool', 'install' }})
    end
end

function Package:merge_stages()
    local collected = {}
    for _, stage in ipairs(self.stages) do
        local target = Target.from_stage(self.name, stage)
        local key = tostring(target)
        if self.stages[key] then
            self.stages[key]:append(target)
        else
            self.stages[key] = target
            table.insert(collected, target)
        end
    end
    self.stages = collected
end

function Package:set_config()
    local config = self.config
    local function non_default_stage(t)
        return t.stage ~= 'clean' and t.stage ~= 'unpack' and t.stage ~= 'patch'
    end
    if config then
        for _, stage in ipairs(filter(non_default_stage, self.stages)) do
            stage.config = stage.config or config
        end
    end
end

function Package:add_toolchain_dependency()
    local function is_build_stage(target)
        return target.stage == 'build'
    end
    for _, stage in ipairs(filter(is_build_stage, self.stages)) do
        table.insert(stage.inputs, 1, Target.new('toolchain'))
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

function Package:type()
    local source = self.source
    return source and source.type
end

function Package:is_source()
    local source_type = Package.type(self)
    return source_type == 'git' or source_type == 'hg'
end

function Package:directory()
    local function basename(location)
        return location and io.popen('basename '..location..' .git'):read()
    end
    if Package.is_source(self) then
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
                command = string.format('%s/$script && touch $out', jagen.bin_dir)
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
    return self:build({
            rule      = 'script',
            outputs   = { tostring(target) },
            inputs    = target.inputs,
            variables = { script = 'jagen-pkg '..target:__tostring(' ') }
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
--{{{ types

Target = {}

function Target.new(name, stage, config)
    local target = { name = name, stage = stage, config = config }
    setmetatable(target, Target)
    Target.__index = Target
    return target
end

function Target.from_list(list)
    return Target.new(list[1], list[2], list[3])
end

function Target.from_stage(name, rule)
    local stage, config

    if type(rule[1]) == 'string' then
        stage = rule[1]
        table.remove(rule, 1)
    end
    if type(rule[1]) == 'string' then
        config = rule[1]
        table.remove(rule, 1)
    end

    local target = Target.new(name, stage, config)
    target.inputs = map(Target.from_list, rule)

    return target
end

function Target.new_from_arg(arg)
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
    self.inputs = append(self.inputs, target.inputs)
    return self
end

--}}}
--{{{ jagen

jagen =
{
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
}

jagen.pkg_dir = system.mkpath(jagen.lib_dir, 'pkg')

jagen.cmd = system.mkpath(jagen.lib_dir, 'cmd.sh')
jagen.rules_file = system.mkpath(jagen.lib_dir, 'rules.'..jagen.sdk..'.lua')
jagen.build_file = system.mkpath(jagen.build_dir, 'build.ninja')

function jagen.exec(...)
    return system.exec(jagen.cmd, ...)
end

function jagen.message(...)
    print(string.format('\027[1;34m:::\027[0m %s', concat(...)))
end
function jagen.warning(...)
    print(string.format('\027[1;33m:::\027[0m %s', concat(...)))
end
function jagen.error(...)
    print(string.format('\027[1;31m:::\027[0m %s', concat(...)))
end

function jagen.debug(...)
    if jagen.debug then
        print(string.format('\027[1;36m:::\027[0m %s', concat(...)))
    end
end
function jagen.debug1(...)
    if os.getenv('pkg_debug') >= '1' then
        print(string.format('\027[1;36m:::\027[0m %s', concat(...)))
    end
end
function jagen.debug2(...)
    if os.getenv('pkg_debug') >= '2' then
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

    function env.package(rule, stages)
        local pkg  = Package:load_rule(rule, stages)
        local name = pkg.name
        if packages[name] then
            packages[name]:merge(pkg)
        else
            packages[name] = pkg
            table.insert(packages, pkg)
        end
    end

    local rules = loadfile(jagen.rules_file)
    if rules then
        setfenv(rules, env)
        rules()
    end

    for _, pkg in ipairs(packages) do
        pkg:merge_stages()
        pkg:add_toolchain_dependency()
        pkg:set_config()
        pkg:add_ordering_dependencies()
    end

    return packages
end

function jagen.generate_include_script(pkg)
    local name = pkg.name
    local filename = system.mkpath(jagen.build_include_dir, name .. '.sh')
    local script = Script:new(pkg)

    local f = assert(io.open(filename, 'w+'))
    f:write(tostring(script))
    f:close()
end

function jagen.generate()
    local packages = jagen.load_rules()
    local ninja = Ninja:new()

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
    if self.pkg.config then
        table.insert(script, self:config())
    end
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

function Script:config()
    local config = self.pkg.config
    if config == 'target' then
        return table.concat({
                'use_toolchain target',
                'p_system="$target_system"',
                'p_prefix="$target_prefix"',
                'p_dest_dir="$target_dir"',
            }, '\n')
    elseif config == 'rootfs' then
        return table.concat({
                'use_toolchain target',
                'p_system="$target_system"',
                'p_prefix=""',
                'p_dest_dir="$sdk_rootfs_prefix"',
            }, '\n')
    end
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
        if build.need_libtool then
            table.insert(o, 'p_need_libtool="yes"')
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
        local target = Target.new_from_arg(arg)
        local packages = target.name and { packages[target.name] } or packages
        for _, pkg in ipairs(packages) do
            for _, stage in ipairs(pkg.stages) do
                if match_target(target, stage) then
                    table.insert(targets, stage)
                end
            end
        end
        if #targets == 0 then
            jagen.warning('No packages found for target:', arg)
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

local src = {}

function src.exec_git(p, ...)
    local dir = Package.directory(p)
    return system.exec('git', '-C', dir, ...)
end

function src.popen_git(p, ...)
    local dir = Package.directory(p)
    return io.popen('git -C '..dir..' '..concat(...)):read() or ''
end

function src.exec_hg(p, ...)
    local dir = Package.directory(p)
    return system.exec('hg', '-R', dir, ...)
end

function src.popen_hg(p, ...)
    local dir = Package.directory(p)
    return io.popen('hg -R '..dir..' '..concat(...)):read() or ''
end

function src.head(p)
    local kind = Package.type(p)
    if kind == 'git' then
        return src.popen_git(p, 'rev-parse', 'HEAD')
    elseif kind == 'hg' then
        return src.popen_hg(p, 'id', '-i')
    end
end

function src.dirty(p)
    local kind = Package.type(p)
    if kind == 'git' then
        return string.len(src.popen_git(p, 'status', '--porcelain')) > 0
    elseif kind == 'hg' then
        return string.len(src.popen_hg(p, 'status')) > 0
    end
end

function src.status(args)
    local packages = jagen.load_rules()
    local source_packages = filter(Package.is_source, packages)

    for _, p in ipairs(source_packages) do
        local dirty = src.dirty(p) and 'dirty' or ''
        print(string.format("%s: %s %s", p.name, src.head(p), dirty))
    end
end

function src.name(filename)
    local name = string.match(filename, '^.*/(.+)') or filename
    local function m(ext)
        return string.match(name, '^([%w_.-]+)'..ext)
    end
    print(m('%.tar') or m('%.tgz') or m('%.tbz2') or m('%.txz') or m('%.zip'))
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
        jagen.message('Available src subcommands: status, name')
    elseif subcommand == 'status' then
        src.status(args)
    elseif subcommand == 'name' then
        src.name(unpack(args))
    else
        jagen.die('Unknown src subcommand:', subcommand);
    end
else
    jagen.die('Unknown command:', command)
end

os.exit(status % 0xFF)
