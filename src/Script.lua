
Script = {}

function Script:new(pkg)
    local script = { pkg = pkg }
    setmetatable(script, self)
    self.__index = self
    return script
end

function Script:write()
    local pkg  = self.pkg
    local path = system.mkpath(jagen.include_dir, pkg:qname()..'.sh')
    local file = assert(io.open(path, 'w+'))
    local function w(format, ...)
        file:write(string.format(format, ...))
    end
    w('#!/bin/sh\n')
    do
        local source = pkg.source
        if source.type and source.location then
            w('\npkg_source="%s %s"', source.type, source.location)
        end
        if source.branch then
            w('\npkg_source_branch="%s"', source.branch)
        end
        if source.path then
            w('\npkg_source_dir="%s"', source.path)
        end
    end
    -- put install variables before build to allow referencing them from
    -- configure options
    do
        if pkg.install then
            local i = pkg.install
            if i.path then
                w('\npkg_dest_dir="%s"', i.path)
            end
            if i.prefix then
                w('\npkg_prefix="%s"', i.prefix)
            end
        end
    end
    do
        local build_dir

        if pkg.build then
            local build = pkg.build

            if build.type then
                w("\npkg_build_type='%s'", build.type)
            end
            if build.options then
                local o = build.options
                if type(build.options) == 'string' then
                    o = { build.options }
                end
                w('\npkg_options="%s"', table.concat(o, '\n'))
            end
            if build.libs then
                w("\npkg_libs='%s'", table.concat(build.libs, ' '))
            end
            if build.generate then
                w("\npkg_build_generate='yes'")
            end
            if build.in_source then
                build_dir = '$pkg_source_dir'
            end
            if build.directory then
                build_dir = build.directory
            end
        end

        build_dir = build_dir or '$pkg_work_dir${pkg_config:+/$pkg_config}'

        w('\npkg_build_dir="%s"', build_dir)
    end
    if pkg.patches then
        w('\njagen_pkg_apply_patches() {')
        for _, patch in ipairs(self.pkg.patches or {}) do
            local name = patch[1]
            local strip = patch[2]
            w('\n  pkg_patch %d "%s"', strip, name)
        end
        w('\n}')
    end
    file:close()
end
