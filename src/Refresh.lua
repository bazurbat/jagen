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
    local format, mkpath = string.format, System.mkpath

    local engine = Engine:new()
    local packages = engine:load_rules()

    engine:finalize()

    local root_config = engine.packages.jagen

    local build_dir = root_config.build_dir
    local include_dir = root_config.include_dir

    System.mkdir(build_dir, include_dir)

    for pkg in each(packages) do
        local scripts = {
            {
                name     = format('%s.sh', pkg.ref),
                contents = pkg:generate_script()
            },
            {
                name     = format('%s.env.sh', pkg.ref),
                contents = pkg:generate_env_script()
            },
            {
                name     = format('%s.export.sh', pkg.ref),
                contents = pkg:generate_export_script()
            }
        }

        for _, script in ipairs(scripts) do
            if script.contents then
                local path = mkpath(include_dir, script.name)
                local file = assert(io.open(path, 'w'))
                file:write(script.contents)
                file:close()
            end
        end

        pkg.script = mkpath(include_dir, scripts[1].name)
    end

    Ninja.generate(packages, root_config)

    local autocomplete = {
        names     = { name = '.jagen-names'     },
        scm_names = { name = '.jagen-scm-names' },
        configs   = { name = '.jagen-configs'   },
        targets   = { name = '.jagen-targets'   },
    }

    for pkg in each(packages) do
        if not pkg.abstract then
            append(autocomplete.names, pkg.name)
            if pkg.source and pkg.source.scm then
                append(autocomplete.scm_names, pkg.name)
            end
            for stage in pairs(pkg.stage or {}) do
                append(autocomplete.targets, pkg.name..':'..stage)
            end
        end
    end

    for _, item in pairs(autocomplete) do
        table.sort(item)
        System.write_file(mkpath(build_dir, item.name), table.concat(item, '\n'))

    end
end

return Refresh
