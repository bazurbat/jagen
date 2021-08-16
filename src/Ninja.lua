local P = {}
local System = require 'System'
local Target = require 'Target'

local format = string.format
local concat = table.concat

local packages = {}
local ninja = {
    supports_console_pool = true
}

local function check_ninja_features()
    local version = os.getenv('jagen_ninja_version')
    if not version then return end
    local major, minor = version:match('(%d+)%.(%d+)%.%d+')
    major, minor = tonumber(major), tonumber(minor)
    if major and minor then
        if major == 1 and minor < 5 then
            ninja.supports_console_pool = false
        end
    end
end

local function dsp(arg)
end

local function fmt(args)
    for i, arg in ipairs(args) do
        if type(arg) == 'function' then
            args[i] = arg()
        end
    end
    return table.concat(args)
end

local function cat(...)
    return fmt({...})
end

local function comp(...)
    local args = {...}
    return function(value)
        local output = value
        for i = 1, #args do
            local f = args[i]
            if type(f) == 'function' then
                output = f(output)
            end
        end
        return output
    end
end

local function findent(n)
    return function(value)
        return string.format('%s%s', string.rep('^', n), tostring(value))
    end
end

local function fjoin(args, f, sep)
    if f == nil then
        f = function(val) return val end
    end
    return function()
        local out = {}
        for _, arg in ipairs(args) do
            table.insert(out, f(arg))
        end
        return table.concat(out, sep)
    end
end

local function with_prefix(prefix)
    return function(v)
        return string.format('%s%s', prefix, tostring(v))
    end
end

local function with_suffix(suffix)
    return function(v)
        return string.format('%s%s', tostring(v), suffix)
    end
end

local function escaped()
    return function(s)
        s = string.gsub(s, "%$", "$$")
        s = string.gsub(s, " ", "$ ")
        s = string.gsub(s, ":", "$:")
        return s
    end
end

local function indent(n)
    return string.rep(' ', n or 4)
end

local function indented(line, n)
    return concat { indent(n), line }
end

local function separated(str)
    if not string.empty(str) then
        return str..(suffix or ' ')
    else
        return ''
    end
end

local function escape(s)
    s = string.gsub(s, "%$", "$$")
    s = string.gsub(s, " ", "$ ")
    s = string.gsub(s, ":", "$:")
    return s
end

local function nonempty(list)
    local out = {}
    for i = 1, #list do
        local item = list[i]
        if item and item ~= '' then
            table.insert(out, item)
        end
    end
    return out
end

local function join(list, sep)
    return concat(list, sep)
end

local function join_space(list)
    return concat(list, ' ')
end

local function join_nl(list)
    return concat(list, '\n')
end

local function join_escaped(list)
    return concat(list, ' $\n')
end

local function join_quoted(list)
    return join_space(collect(list, map(function (i)
                    return format("'%s'", escape(tostring(i)))
        end)))
end

local function quote(s)
    return format("'%s'", string.gsub(s or '', "%$", "$$"))
end

local function binding(k, v)
    return format('%s = %s', assert(k), tostring(assert(v)))
end

local function format_pool(name, depth)
    return format('pool %s\n%sdepth = %s', name, indent(4), depth)
end

local function format_rule(name, command)
    return format('rule %s\n%scommand = %s', name, indent(4), command)
end

local function format_target(target)
    local output = { target.name, target.stage, target.config }
    return join(output, ':')
end

local function format_outputs(outputs)
    local lines = { escape(outputs[1] or '') }
    if #outputs > 1 then
        extend(lines, map(function (x)
                    return indented(escape(tostring(x)), 6)
            end, sort(table.rest(outputs, 2))))
        append(lines, indent(12))
    end
    return join_escaped(lines)
end

local function format_inputs(inputs)
    local lines = { '' }
    extend(lines, sort(map(function (x)
                    return indented(escape(format_target(x)), 16)
        end, inputs or {})))
    return join_escaped(lines)
end

local function format_refresh(files)
    local outputs = { 'build.ninja' }
    return fmt { '\n', 'build build.ninja: refresh',
        fjoin(files, comp(escape, with_prefix(cat(' $\n', indent(6))))),
        '\n', indent(4), binding('description', 'refresh')
    }
end

local function format_phony(files)
    return format('build %s: phony', format_outputs(files))
end

local function format_build(build)
    return fmt { '\n', 'build ',
        fjoin(build.outputs, comp(escape, with_suffix(' $\n')), indent(6)),
        cat(indent(8), ': stage'),
        fjoin(build.inputs, comp(escape, with_prefix(cat(' $\n', indent(6))))),
        fjoin(map(function (key)
                      return binding(key, build.vars[key])
                  end, sort(table.keys(build.vars))),
              with_prefix(cat('\n', indent(2))))
    }
end

local function format_stage(name, target, pkg)
    local output = { pkg.name, name, pkg.config }
    local args = { pkg.name, name, pkg.config or quote('') }

    local inputs = {}
    for _, item in ipairs(target.inputs or {}) do
        local t = { item.name or pkg.name, item.stage, item.config or pkg.config }
        table.insert(inputs, join(t, ':'))
    end

    local vars = {
        description = join(output, ' '),
        args        = join(args, ' '),
    }

    vars.pool = target.pool

    return format_build {
        rule    = 'stage',
        uses    = {},
        inputs  = inputs,
        outputs = { join(output, ':') },
        vars    = vars
    }
end

local function format_package(pkg)
    local lines  = {}
    local stages = table.keys(pkg.stages)
    table.sort(stages)
    for name in each(stages) do
        append(lines, format_stage(name, pkg.stages[name], pkg))
    end
    return join(lines)
end

local function assign_pools(packages)
    local function is_android_gradle(target, pkg)
        if target.stage == 'compile' then
            local build = pkg:get('build', target.config)
            return build and build.type == 'android-gradle'
        end
    end
    local function is_rust_toolchain(target, pkg)
        if target.stage == 'install' then
            local build = pkg:get('build', target.config)
            return build and build.type == 'rust-toolchain'
        end
    end
    local function is_interactive(target)
        return target.interactive
    end
    local function assign_interactive(targets)
        if table.find(targets, is_interactive) then
            for target in each(targets) do
                target.interactive = true
            end
        end
    end
    local gradle_stages, rust_stages = {}, {}
    for name, pkg in pairs(packages) do
        for target, this in pkg:each() do
            if is_android_gradle(target, pkg) then
                append(gradle_stages, target)
                target.pool = 'gradle_android'
            end
            if is_rust_toolchain(target, pkg) then
                append(rust_stages, target)
                target.pool = 'rust_toolchain'
            end
        end
    end
    assign_interactive(gradle_stages)
    assign_interactive(rust_stages)
    if ninja.supports_console_pool then
        for name, pkg in pairs(packages) do
            for target, this in pkg:each() do
                if is_interactive(target) then
                    target.pool = 'console'
                end
            end
        end
    end
end

function P.generate(out_file, rules)
    check_ninja_features()

    packages = rules
    local file = assert(io.open(out_file, 'w'))

    assign_pools(rules)

    local lines = {
        binding('ninja_required_version', '1.1'),
        binding('builddir', assert(Jagen.build_dir)),
        format_pool('gradle_android', 1),
        format_pool('rust_toolchain', 1),
        format_rule('stage', join {
                separated(Jagen.shell), 'jagen-stage $args'
            }),
        format_rule('refresh', join_space(nonempty { Jagen.shell, System.expand('$jagen_root_dir/jagen'), 'refresh' }))
    }

    local for_refresh = {} -- Jagen:find_for_refresh()
    local include_dir = Jagen.include_dir
    for pkg in each(rules) do
        append(for_refresh, System.mkpath(include_dir, string.format('%s.sh', pkg.ref)))
    end

    append(lines, format_refresh(for_refresh))
    append(lines, format_phony(for_refresh))

    extend(lines, map(format_package, rules))

    file:write(join_nl(lines))
    file:write('\n')

    file:close()
end

return P
