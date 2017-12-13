local System = require 'System'

local P = {}

local function write_env(w, pkg)
    local env = pkg.env or { pkg.config }
    for _, e in ipairs(env) do
        w('use_env %s || true', e)
    end
end

local function write_source(w, pkg)
    local source = pkg.source
    if not source then return end

    if source.type and source.location then
        w('pkg_source="%s %s"', source.type, source.location)
    end

    if source.filename then
        w('pkg_source_filename="%s"', source.filename)
    end
    if source.basename then
        w('pkg_source_basename="%s"', source.basename)
    end

    if source.sha1sum then
        w('pkg_source_sha1sum="%s"', source.sha1sum)
    end
    if source.sha256sum then
        w('pkg_source_sha256sum="%s"', source.sha256sum)
    end
    if source.md5sum then
        w('pkg_source_md5sum="%s"', source.md5sum)
    end

    if source.branch then
        w('pkg_source_branch="%s"', source.branch)
    end

    if source.dir then
        w('pkg_source_dir="%s"', source.dir)
    end

    if source.exclude then
        w("pkg_source_exclude='yes'")
    end
    if source.ignore_dirty then
        w("pkg_source_ignore_dirty='yes'")
    end

    if pkg.patches then
        local patches = pkg.patches
        if patches.required then
            w('pkg_patches_required="%s"', table.concat(patches.required, '\n'))
        end
        if patches.provided then
            w('pkg_patches_provided="%s"', table.concat(patches.provided, '\n'))
        end
        if #patches > 0 then
            assert(patches.required and #patches == #patches.required)
            w('jagen_pkg_apply_patches() {')
            for i, item in ipairs(patches) do
                local name = item[1]
                local strip = item[2]
                w('  pkg_run_patch %d "%s"', strip, patches.required[i])
            end
            w('}')
        end
    end
end

local function write_build(w, pkg)
    local build = pkg.build
    if not build then return end

    local build_dir

    if build.type then
        w("pkg_build_type='%s'", build.type)
    end

    if build.type == 'CMake' then
        if build.generator then
            w('pkg_build_generator="%s"', build.generator)
        end
    end

    if build.generate then
        w("pkg_build_generate='yes'")
    end

    if build.cmake_module_path then
        w('pkg_cmake_module_path="%s"', build.cmake_module_path)
    end

    if build.cmake_toolchain_file then
        w('pkg_cmake_toolchain_file="%s"', build.cmake_toolchain_file)
    end

    if build.configure_file then
        w('pkg_configure_file="%s"', build.configure_file)
    end

    if build.configure_needs_install_dir then
        w("pkg_configure_needs_install_dir='yes'")
    end

    if build.profile then
        w("pkg_build_profile='%s'", build.profile)
    end

    if build.options then
        local o = build.options
        if type(build.options) == 'string' then
            o = { build.options }
        end
        w('pkg_build_options="%s"', table.concat(o, '\n'))
    end

    if build.libs then
        w("pkg_libs='%s'", table.concat(build.libs, ' '))
    end

    if build.work_dir then
        w('pkg_work_dir="%s"', build.work_dir)
    end

    if build.in_source then
        w("pkg_build_in_source='yes'")
        build_dir = '$pkg_source_dir'
    end

    if build.dir then
        build_dir = build.dir
    end

    if build_dir then
        w('pkg_build_dir="%s"', build_dir)
    end
end

local function write_install(w, pkg)
    local install = pkg.install
    if not install then return end

    if install.type then
        w('pkg_install_type="%s"', install.type)
    end

    if install.root then
        w('pkg_install_root="%s"', install.root)
    end

    if install.prefix then
        w('pkg_install_prefix="%s"', install.prefix)
    end

    if install.config_script then
        w("pkg_install_config_script='%s'", install.config_script)
    end

    if install.args then
        local args
        if type(install.args) == 'table' then
            args = install.args
        else
            args = { tostring(install.args) }
        end
        w('pkg_install_args="%s"', table.concat(args, '\n'))
    end

    if install.ldconfig then
        w("pkg_install_ldconfig='yes'")
    end

    if install.modules then
        if type(install.modules) == 'string' then
            w("pkg_install_modules_dirs='%s'", install.modules)
        elseif type(install.modules) == 'table' then
            w('pkg_install_modules_dirs="%s"',
                table.concat(install.modules, ' '))
        end
    end
end

local function generate_script(filename, pkg)
    local file = assert(io.open(filename, 'w+'))

    local function w(format, ...)
        file:write(string.format(format..'\n', ...))
    end

    write_env(w, pkg)
    write_source(w, pkg)
    -- write install first to allow referencing dest dir and such from
    -- configure options
    write_install(w, pkg)
    write_build(w, pkg)

    file:close()
end

function P:generate(pkg, dir)
    local filename = System.mkpath(dir, string.format('%s.sh', pkg.name))
    generate_script(filename, pkg)
    for name, config in pairs(pkg.configs) do
        filename = System.mkpath(dir, string.format('%s__%s.sh', pkg.name, name))
        generate_script(filename, config)
    end
end

return P
