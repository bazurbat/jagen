
function read_package(rule)
    local stages = {}
    for i, s in ipairs(rule) do
        table.insert(stages, s)
        rule[i] = nil
    end
    rule.stages = stages
    return rule
end

function load_rules(pathname)
    local rules = dofile(pathname)

    local function load_package(pkg_rule)
        local package = {}
        local tmp = {}
        local collected = {}

        local function load_source(source)
            if type(source) == 'string' then
                return { type = 'dist', location = source }
            else
                return source
            end
        end

        local function getkey(name, config)
            if config then
                return name .. ':' .. config
            else
                return name
            end
        end

        local function input_to_target(d)
            return target.new(d[1], d[2], d[3])
        end

        local function load_stage(stage_rule)
            local stage, config

            if type(stage_rule[1]) == 'string' then
                stage = stage_rule[1]
                table.remove(stage_rule, 1)
            end
            if type(stage_rule[1]) == 'string' then
                config = stage_rule[1]
                table.remove(stage_rule, 1)
            end

            local key = getkey(stage, config)
            local inputs = map(input_to_target, list(stage_rule))
            
            if tmp[key] then
                tmp[key].inputs = append(tmp[key].inputs or {}, inputs)
            else
                local target = target.new(pkg_rule.name, stage, config)
                target.inputs = inputs
                tmp[key] = target
                table.insert(collected, target)
            end
        end

        function add_previous(stages)
            local prev, common

            for _, s in ipairs(stages) do
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

        for_each(pkg_rule.stages, load_stage)
        add_previous(collected)

        package.name = pkg_rule.name
        package.source = load_source(pkg_rule.source)
        package.patches = pkg_rule.patches
        package.stages = collected

        -- print("===", package.name)
        -- pretty.dump(package)

        return package
    end

    local packages = map(load_package, rules)

    return packages
end
