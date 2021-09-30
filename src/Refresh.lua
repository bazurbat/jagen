local Command = require 'Command'
local Engine  = require 'Engine'
local Ninja   = require 'Ninja'
local Script  = require 'Script'
local System  = require 'System'
local Target  = require 'Target'

local Refresh = {}

local function generate_cargo_config(packages)
    local targets, lines = {}, {}
    for name, pkg in pairs(packages) do
        for this, config in pkg:each_config() do
            local build = this.build
            if build.type == 'rust' then
                local system = pkg:get_build('system', config)
                local cc = pkg:get_build('cc', config)
                    or pkg:get_toolchain_build('cc', config, packages)
                    or 'gcc'
                local toolchain_system = pkg:get_toolchain_build('system', config, packages)
                if system and cc and toolchain_system then
                    targets[system] = string.format('%s-%s', toolchain_system, cc)
                end
            end
        end
    end
    for target, path in pairs(targets) do
        table.insert(lines, string.format('[target.%s]\nlinker = "%s"', target, path))
    end
    local config_dir = assert(os.getenv('jagen_cargo_config_dir'))
    local config_path = System.mkpath(config_dir, 'config')
    System.mkdir(config_dir)
    local file = assert(io.open(config_path, 'w'))
    file:write(table.concat(lines, '\n'), '\n')
    file:close()
end

function Refresh:run(args)
    local engine = Engine:new()
    local packages = engine:load_rules()

    engine:finalize()

    -- print(pretty(packages))

    -- print(pretty(engine.config.jagen))

    local dir = engine.config.jagen.dir

    -- System.mkdir(dir.build, dir.include, dir.log)
    System.mkdir(dir.build, dir.include)

    local targets = {}

    for pkg in each(packages) do
        local filename = System.mkpath(dir.include, string.format('%s.sh', pkg.name))
        Script:write(pkg, filename)

        for name, stage in pairs(pkg.stages or {}) do
            local target = Target.from_args(pkg.name, name, pkg.config)
            append(targets, target)
            -- local filename = string.format('%s/%s', dir.log, target:log_filename())
            -- assert(io.open(filename, 'a+')):close()
        end
    end

    for name, config in pairs(engine.config) do
        local filename = System.mkpath(dir.include, string.format('%s.config.sh', name))
        if name ~= 'jagen' then
            name = 'jagen_'..name
        end
        Script:write(config, filename, name)
    end

    Ninja.generate(Jagen.build_file, packages)
end

return Refresh
