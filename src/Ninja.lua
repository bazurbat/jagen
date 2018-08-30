local P = {}
local System = require 'System'
local Target = require 'Target'

local format = string.format
local concat = table.concat

local packages = {}

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

local function join(list)
    return concat(list)
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

local function format_build(build)
    local lines = { '' }

    local function format_outputs(outputs)
        local lines = { escape(outputs[1]) }
        if #outputs > 1 then
            extend(lines, map(function (x)
                        return indented(escape(tostring(x)), 8)
                end, sort(table.rest(outputs, 2))))
            append(lines, indent(12))
        end
        return join_escaped(lines)
    end

    local function format_inputs(inputs)
        local lines = { '' }
        extend(lines, sort(map(function (x)
                        return indented(escape(tostring(x)), 16)
            end, inputs or {})))
        return join_escaped(lines)
    end

    local function format_uses(uses)
        local lines = {}
        if #uses > 0 then
            append(lines, ' ||')
            extend(lines, sort(map(function (x)
                            return indented(escape(tostring(x)), 16)
                end, uses or {})))
        end
        return join_escaped(lines)
    end

    append(lines, format('build %s: %s%s%s',
            format_outputs(build.outputs),
            assert(build.rule),
            format_inputs(build.inputs),
            format_uses(build.uses)))

    extend(lines, map(function (key)
                return indented(binding(key, build.vars[key]))
        end, sort(table.keys(build.vars))))

    return join_nl(lines)
end

local function format_stage(target, pkg)
    local config = target.config

    local function get_outputs()
        local outputs = { tostring(target) }
        return extend(outputs, target.outputs or {})
    end

    local function format_args()
        local command = { target.name, target.stage,
            config or quote('')
        }
        local arg = target.arg
        if type(arg) == 'string' or type(arg) == 'number' then
            arg = { tostring(arg) }
        end
        if type(arg) == 'table' then
            append(command, join_quoted(target.arg))
        end
        return join_space(command)
    end

    local vars = {
        description = target:__tostring(' '),
        args        = format_args(),
    }

    local uses = {}

    if config then
        local this = assert(pkg.configs[config])
        for spec in each(pkg.uses or {}, this.uses) do
            local use = Target.from_use(spec)
            local used = packages[use.name]
            if used then
                local config = use.config or used:has_config(config) and config
                if config then
                    append_uniq(Target.from_args(use.name, 'export', config), uses)
                end
            end
        end
    else
        for use in each(pkg.uses or {}) do
            append_uniq(Target.from_args(Target.from_use(use).name, 'export'), uses)
        end
        for this in pkg:each_config() do
            for use in each(this.uses or {}) do
                append_uniq(Target.from_args(Target.from_use(use).name, 'export'), uses)
            end
        end
    end

    if target.stage == 'provide_patches' then
        -- Each output whose modification time the command did not change will
        -- be treated as though it had never needed to be built. This means
        -- that the packages which require the provided patches will no be
        -- rebuilt if the patch files themselves have not changed even though
        -- the provide_patches stage execution making them "dirty".
        vars.restat = 'true'
    end

    if target.stage == 'compile' then
        local build = pkg:get('build', target.config)
        if build and build.type == 'android-gradle' then
            vars.pool = 'gradle_android'
        end
    end

    if target.stage == 'install' then
        local build = pkg:get('build', target.config)
        if build and build.type == 'rust-toolchain' then
            vars.pool = 'rust_toolchain'
        end
    end

    return format_build {
        rule    = 'stage',
        uses    = uses,
        inputs  = target.inputs,
        outputs = get_outputs(),
        vars    = vars
    }
end

local function format_package(name, pkg)
    local lines = {}
    for stage in pkg:each() do
        append(lines, format_stage(stage, pkg))
    end
    return join(lines)
end

function P.generate(out_file, rules)
    packages = rules
    local file = assert(io.open(out_file, 'w'))
    local sorted_rules = sort(table.tolist(rules),
        function (a, b)
            return a.name < b.name
        end)

    local lines = {
        binding('ninja_required_version', '1.1'),
        binding('builddir', assert(Jagen.build_dir)),
        format_pool('gradle_android', 1),
        format_pool('rust_toolchain', 1),
        format_rule('stage', join {
                separated(Jagen.shell), 'jagen-stage $args'
            })
    }

    extend(lines, pmap(format_package, sorted_rules))

    file:write(join_nl(lines))
    file:write('\n')

    file:close()
end

return P
