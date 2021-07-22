config { 'default' }

config { 'pkg', extends = 'default',
    work_dir = '${jagen.build_dir}/${name}',
    install = {
        dir = '${install.root}${install.prefix}'
    }
}

config { 'host', extends = 'pkg',
    install = {
        prefix = '${jagen.host_dir}',
        root = ''
    },
    env = {
        PKG_CONFIG_PATH = '${install.prefix}/lib/pkgconfig'
    }
}

config { 'target', extends = 'pkg',
    install = {
        prefix = '',
        root = '${jagen.target_dir}'
    },
    env = {
        PKG_CONFIG_SYSROOT_DIR = "${install.root}",
        PKG_CONFIG_LIBDIR = "${install.root}/lib/pkgconfig",
        PKG_CONFIG_PATH = "${install.root}/usr/lib/pkgconfig",

        -- pkg-config tries to be smart and removes -I and -L flags from it's
        -- output when they resemble system paths. This causes SYSROOT_DIR to
        -- not be added to them, which prevents packages to find each other
        -- when building the sysroot itself.
        PKG_CONFIG_ALLOW_SYSTEM_CFLAGS = 1,
        PKG_CONFIG_ALLOW_SYSTEM_LIBS = 1
    }
}

config { 'target',
    build = {
        cmake = {
            options = {
                '-DCMAKE_FIND_ROOT_PATH="${install.root}"',
                '-DCMAKE_FIND_ROOT_PATH_MODE_PROGRAM=NEVER',
                '-DCMAKE_FIND_ROOT_PATH_MODE_LIBRARY=ONLY',
                '-DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE=ONLY',
                '-DCMAKE_FIND_ROOT_PATH_MODE_PACKAGE=ONLY'
            }
        }
    }
}
