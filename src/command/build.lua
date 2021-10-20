local Command = require 'Command'
local Engine  = require 'Engine'
local Log     = require 'Log'
local Options = require 'Options'
local System  = require 'System'
local Target  = require 'Target'

local P = {}

local function write_targets(targets, args, build_targets_file)
    local has_console = os.getenv('jagen__has_console')
    local is_interactive = has_console and not args['quiet'] and not (args['progress'] or args['follow'] or args['follow-all'])
    local curr_list, saved_list, is_eq = {}, {}, true

    if is_interactive then
        for target, explicit in pairs(targets) do
            if explicit then
                append(curr_list, target)
            end
        end
        sort(curr_list)
    end

    local file = io.open(build_targets_file)
    if file then
        for line in file:lines() do
            append(saved_list, line)
        end
        file:close()
    end

    if #curr_list ~= #saved_list then
        is_eq = false
    else
        for i = 1, #curr_list do
            if curr_list[i] ~= saved_list[i] then
                is_eq = false
                break
            end
        end
    end

    if not is_eq then
        local file = assert(io.open(build_targets_file, 'w'))
        for target in each(curr_list) do
            file:write(target, '\n')
        end
        file:close()
    end
end

function P:run(args)
    local options = Options:new {
        { 'help,h' },
        { 'match,m' },
        { 'clean,c' },
        { 'clean-ignored,C' },
        { 'all,a' },
        { 'no-rebuild,n' },
        { 'progress,p' },
        { 'follow,f' },
        { 'follow-all,F' },
        { 'quiet,q' },
        { 'exclude,x' },
        { 'ignore-dirty,y' },
        { 'ignore-exclude,Y' },
        { 'no-auto' }
    }
    args = options:parse(args)
    if not args then return false end
    if args['help'] then
        return Jagen.command['help']({ 'build' }, args['help'].short)
    end

    local engine = Engine:new()
    local packages = engine:load_rules()
    -- if not Engine:validate() then
    --     Log.error('aborting the build due to rule errors')
    --     return false
    -- end

    local targets, arg_clean = {}, args['clean'] or args['clean-ignored']

    if #args == 0 then
        if not (args['progress'] or args['follow'] or args['follow-all']) then
            args['quiet'] = true
        end
        -- for name in kvpairs(packages) do
        --     append(args, name)
        -- end
    end

    local function match(s, p)
        return s == 'clean' and p == '^clean$' or
               s ~= 'clean' and (not p or p and s:match(p))
    end

    local not_found_args = {}
    for arg in each(args) do
        local found = false
        local parts = arg:split(':')
        local name_pat, stage_pat, config_pat = unpack(map(string.to_pattern, parts))

        local function by_name(pkg)
            return match(pkg.name, name_pat)
        end

        for name, pkg in filter(by_name)(kvpairs(packages)) do
            for stage in kvpairs(pkg.stages or {}) do
                local target = Target.from_args(name, stage)
                if arg_clean and stage == 'clean' then
                    targets[tostring(target)] = true
                elseif match(stage, stage_pat) then
                    targets[tostring(target)] = true
                    found = true
                end
            end
            if not found then
                append(not_found_args, arg)
            end
        end
    end

    for arg in each(not_found_args) do
        Log.warning('could not find targets matching: %s', arg)
    end

    if args['match'] then
        local keys = table.keys(targets)
        table.sort(keys)
        for key in each(keys) do
            local explicit = targets[key]
            print(string.format('%s%s', key, not explicit and ' *' or ''))
        end
        return true
    end

    -- some targets were specified but none matched, consider this an error
    if #args > 0 and not next(targets) then
        return false
    end

    local config = engine.config.root
    local cmd = assert(engine.config.self.cmd)

    write_targets(targets, args, assert(config.build_targets_file))

    local args_path = System.mkpath(assert(config.build_dir), '.build-args')
    local args_file = assert(io.open(args_path, 'w'))
    if args._args then
        args_file:write(table.concat(args._args, '\n'))
    end
    args_file:close()

    local ok = Command:new(quote(cmd), 'build', tostring(args), unpack(table.keys(targets))):exec()

    -- io.open(args_path, 'w'):close()

    return ok
end

return P
