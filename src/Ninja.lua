local P = {}
local System = require 'System'

local format = string.format
local concat = table.concat

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
    return join_space(newlist(map(function (i)
                    return format("'%s'", escape(tostring(i)))
        end), list))
end

local function quote(s)
    return format("'%s'", string.gsub(s or '', "%$", "$$"))
end

local function binding(k, v)
    return format('%s = %s', assert(k), tostring(assert(v)))
end

local function format_rule(name, command)
    return format('rule %s\n%scommand = %s', name, indent(4), command)
end

local function format_build(build)
    local lines = { '' }

    local function format_outputs(outputs)
        local lines = { outputs[1] }
        if #outputs > 1 then
            extend(lines, map(function (x)
                        return indented(escape(x), 8)
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

    append(lines, format('build %s: %s%s',
            format_outputs(build.outputs),
            assert(build.rule),
            format_inputs(build.inputs)))

    extend(lines, map(function (key)
                return indented(binding(key, build.vars[key]))
        end, sort(table.keys(build.vars))))

    return join_nl(lines)
end

local function format_stage(target)
    local function get_outputs()
        local outputs = { tostring(target) }
        return extend(outputs, target.outputs or {})
    end

    local function format_args()
        local command = { target.name, target.stage,
            target.config or quote('')
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

    if target.stage == 'provide_patches' then
        -- Each output whose modification time the command did not change will
        -- be treated as though it had never needed to be built. This means
        -- that the packages which require the provided patches will no be
        -- rebuilt if the patch files themselves have not changed even though
        -- the provide_patches stage execution making them "dirty".
        vars.restat = 'true'
    end

    return format_build {
        rule    = 'stage',
        inputs  = target.inputs,
        outputs = get_outputs(),
        vars    = vars
    }
end

local function format_package(name, pkg)
    local lines = {}
    for stage in pkg:each() do
        append(lines, format_stage(stage))
    end
    return join(lines)
end

function P.generate(out_file, rules)
    local file = assert(io.open(out_file, 'w'))
    local packages = {}

    for k, v in pairs(rules) do
        table.insert(packages, v)
    end

    table.sort(packages, function (a, b)
            return a.name < b.name
        end)

    local lines = {
        binding('builddir', assert(Jagen.build_dir)),
        format_rule('stage', join {
                separated(Jagen.shell), 'jagen-pkg $args'
            })
    }

    extend(lines, pmap(format_package, packages))

    file:write(join_nl(lines))
    file:write('\n')

    file:close()
end

return P
