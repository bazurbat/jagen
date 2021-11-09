config { 'jagen',
    dir         = os.getenv('jagen_dir'),
    root_dir    = os.getenv('jagen_root_dir'),
    build_dir   = os.getenv('jagen_build_dir'),
    include_dir = os.getenv('jagen_include_dir'),
    bin_dir     = '${root_dir}/bin',
    dist_dir    = '${root_dir}/dist',
    lib_dir     = '${root_dir}/lib',
    log_dir     = '${root_dir}/log',
    src_dir     = '${root_dir}/src',
    build_file  = '${build_dir}/build.ninja',
    build_targets_file = '${build_dir}/.build-targets',
    build_args_file = '${build_dir}/.build-args',
    self = {
        bin_dir = '${dir}/bin',
        src_dir = '${dir}/src',
        cmd     = '${self.src_dir}/cmd.sh',
    },
    env = {
        -- Disable passphrase querying.
        GIT_SSH_COMMAND = 'ssh -o BatchMode=yes',
        -- Do not prompt on the terminal (e.g. when asking for HTTP credentials).
        GIT_TERMINAL_PROMPT = 0,
        -- Never install the translations.
        LINGUAS = '',
        -- Do not run tools with an interactive graphical UI.
        DISPLAY = ''
    },
    toolchain = {
        c_compiler_names = { 'gcc', 'clang' },
        cxx_compiler_names = { 'g++', 'c++', 'clang++' },
        cpp_names = { 'cpp' },
        linker_names = { 'ld' }
    }
}

config { 'host',
    dir = '${jagen:root_dir}/host',
}

config { 'target',
    dir = '${jagen:root_dir}/target'
}

config { 'ccache',
    env = {
        jagen_ccache="${jagen_ccache:-ccache}",

        -- This should allow sharing cache between different roots.
        CCACHE_BASEDIR="$HOME",

        -- The default check uses compiler's mtime and size, which can cause
        -- false misses due to autogenerated wrappers for some SDKs.
        CCACHE_COMPILERCHECK="content",

        -- Performance improvements (read the manual).
        CCACHE_SLOPPINESS="file_stat_matches,include_file_mtime,time_macros"
    }
}

template {
    parse = true,
    match = { as 'name', anyof { as 'config', none } },
    apply = {
        none, none,
        name   = as 'name',
        config = as 'config'
    }
}

template {
    parse = true,
    match = {
        name = as 'name'
    },
    apply = {
        ref = as 'name'
    }
}

template {
    parse = true,
    match = {
        name   = as 'name',
        config = as 'config'
    },
    apply = {
        ref = cat(as 'name', ':', as 'config')
    }
}

template {
    parse = true,
    match = { class = bind(value, oftype 'string') },
    apply = { class = { value } }
}

template {
    parse = true,
    match = { apply = bind(value, oftype 'string') },
    apply = { apply = { value } }
}

template {
    parse = true,
    match = { build = bind(value, oftype 'string') },
    apply = { build = { type = value } }
}

template {
    parse = true,
    match = { build = { bind(value, oftype 'string') } },
    apply = { build = { none, type = value } }
}

template {
    parse = true,
    match = { install = bind(value, oftype 'string') },
    apply = { install = { type = value } }
}

template {
    parse = true,
    match = { install = { bind(value, oftype 'string') } },
    apply = { install = { none, type = value } }
}

-- source

template {
    match = { source = { type = some } },
    apply = { source = { scm = false } }
}

template {
    match = { source = { type = anyof { 'git', 'hg' } } },
    apply = { source = { scm = true } }
}

template {
    match = { source = { name = anyof { value, none } } },
    apply = { source = { name = anyof { value, '${name}' } } }
}

template {
    match = {
        source = {
            location = value 'location',
            filename = anyof { value 'filename', none }
        }
    },
    apply = {
        source = {
            filename = anyof {
                value 'filename',
                bind(value 'location', match '^.*/(.+)$'),
                value 'location' },
            basename = bind(value 'location',
                            anyof { match '^.*/(.+)%.tar%.%w+$',
                                    match '^.*/(.+)%.[^.]+$',
                                    match '^.*/(.+)$',
                                    match '.*' })
        }
    }
}

template {
    match = {
        source = {
            type = 'dir',
            location = value,
        }
    },
    apply = {
        source = { dir = value }
    }
}

template {
    match = {
        source = {
            location = some,
            dir = none,
            scm = true,
            name = as 'name'
        }
    },
    apply = {
        source = {
            dir = cat('${jagen:src_dir}', '/', value 'name')
        }
    }
}

template {
    match = {
        source = {
            location = some,
            dir = none,
            scm = false,
            name = as 'name'
        }
    },
    apply = {
        source = {
            dir = cat('${jagen:build_dir}', '/', value 'name')
        }
    }
}

template {
    match = { source = { dir = value } },
    apply = {
        export = {
            source = { dir = value },
        }
    }
}

-- work_dir

template {
    match = { config = none },
    apply = {
        work_dir = '${jagen:build_dir}/${name}',
    }
}

template {
    match = { config = some },
    apply = {
        work_dir = '${jagen:build_dir}/${name}:${config}',
    }
}

-- clean

template {
    match = { source = some },
    apply = { stages = { clean = {} } }
}

-- update

template {
    match = { source = { type = anyof { 'git', 'hg' } } },
    apply = {
        stages = {
            update = { inputs = { stage 'clean' } }
        }
    }
}

-- unpack

template {
    match = { source = { type = '^dist' } },
    apply = {
        stages = {
            unpack = { inputs = { stage 'clean' } }
        }
    }
}

-- patch

template {
    match = {
        patches = some,
        stages  = { update = some }
    },
    apply = {
        stages = {
            patch = { inputs = { stage 'update' } }
        }
    }
}

template {
    match = {
        patches = some,
        stages  = { unpack = some }
    },
    apply = {
        stages = {
            patch = { inputs = { stage 'unpack' } }
        }
    }
}

-- build

template {
    match = { build = some },
    apply = {
        build = { dir = '${work_dir}' },
        export = {
            build = {
                dir = '${build.dir}'
            }
        }
    }
}

-- template {
--     match = { install = { type = 'linux-kernel' } },
--     apply = {
--         -- TODO: if not in source
--         KBUILD_OUTPUT = "${build.dir}"
--     }
-- }

-- stage: configure

template {
    match = {
        patches = some,
        build = { type = some }
    },
    apply = {
        stages = {
            configure = { inputs = { stage 'patch' } }
        }
    }
}

template {
    match = {
        build   = { type = some },
        stages  = { unpack = some },
        patches = none,
    },
    apply = {
        stages = {
            configure = { inputs = { stage 'unpack' } }
        }
    }
}

template {
    match = {
        build   = { type = some },
        stages  = { update = some },
        patches = none,
    },
    apply = {
        stages = {
            configure = { inputs = { stage 'update' } }
        }
    }
}

-- stage: compile

template {
    match = { build = { type = some } },
    apply = {
        stages = {
            compile = { inputs = { stage 'configure' } }
        }
    }
}

-- install

template {
    match = {
        build   = { type = value },
        install = anyof { none, { type = none } }
    },
    apply = {
        install = { type = value }
    }
}

-- stage: install

template {
    match = { install = { type = some } },
    apply = { stages  = { install = {} } }
}

template {
    match = {
        build   = anyof { none, { type = none } },
        install = { type = some },
        stages  = { unpack = some }
    },
    apply = {
        stages = {
            install = { inputs = { stage 'unpack' } }
        }
    }
}

template {
    match = {
        build   = anyof { none, { type = none } },
        install = { type  = some },
        stages  = { clean = some }
    },
    apply = {
        stages = {
            install = { inputs = { stage 'clean' } }
        }
    }
}

template {
    match = {
        install = { type    = some },
        stages  = { compile = some }
    },
    apply = {
        stages = {
            install = { inputs = { stage 'compile' } }
        }
    }
}

-- host

template {
    match = { class = contains 'host' },
    apply = {
        install = {
            prefix = '${host:dir}',
            root = ''
        }
    }
}

template {
    match = {
        install = {
            prefix = some,
            root = none
        }
    },
    apply = {
        env = {
            PKG_CONFIG_PATH = '${install.prefix}/lib/pkgconfig'
        }
    }
}

-- target

template {
    match = { class = contains 'target' },
    apply = {
        install = {
            prefix = '',
            root = '${target:dir}'
        }
    }
}

template {
    match = {
        install = {
            prefix = none,
            root = some
        }
    },
    apply = {
        install = {
            prefix = '',
            root = '${target:dir}'
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
}

template {
    match = {
        build   = { type = 'cmake' },
        install = { root = some    }
    },
    apply = {
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
}

template {
    match = {
        class = contains 'target',
        build = { type = 'kbuild' },
    },
    env = {
        ARCH="$pkg_build_arch",
        CROSS_COMPILE="$pkg_build_toolchain_prefix",
    }
}

-- install.dir

template {
    match = { install = { root = some, prefix = some } },
    apply = {
        install = { dir = '${install.root}${install.prefix}' }
    }
}

template {
    match = { install = { dir = value } },
    apply = {
        export = {
            install = { dir = value }
        }
    }
}

-- toolchain

template {
    match = {
        build = {
            type = some,
            toolchain = none
        }
    },
    apply = {
        build = {
            toolchain = 'system-native'
        }
    }
}

template {
    match = { build = { toolchain = value } },
    apply = { uses  = { value } }
}

template {
    match = {
        build = { cflags = value, cxxflags = none }
    },
    apply = {
        build = { cxxflags = value }
    }
}

template {
    match = {
        build = {
            toolchain = value 'toolchain',
            system    = anyof { value 'system', none },
        }
    },
    apply = {
        build = {
            system = anyof { value 'system', from(value 'toolchain', 'export.system') },
            arch   = from(value 'toolchain', 'export.arch'),
            cpu    = from(value 'toolchain', 'export.cpu'),
        }
    }
}

template {
    match = {
        build = { system = value, arch = none }
    },
    apply = {
        build = {
            arch = bind(value, match('^(%w+)-?'))
        }
    }
}

template {
    match = {
        build = { system = value }
    },
    apply = {
        build = {
           toolchain_prefix = cat(value, '-')
        }
    }
}

template {
    match = {
        build = {
            type = 'cmake',
            toolchain = as 'toolchain',
            cmake_executable = anyof { value 'cmake_executable', none }
        }
    },
    apply = {
        build = {
            cmake_executable = anyof {
                value 'cmake_executable',
                from(value 'toolchain', 'export.cmake_executable'),
                'cmake'
            }
            -- cmake_generator      = 'g',
            -- cmake_options        = 'o',
            -- cmake_module_path    = 'm',
            -- cmake_toolchain_file = 't'
        }
    }
}


template {
    match = { build = { toolchain = value } },
    apply = {
        env = {
            -- CMake honors CC and CXX but not LD. It uses compiler for linking.
            CC      = 'gcc',
            CXX     = 'g++',
            CPP     = 'cpp',
            LD      = 'ld',
            -- Some of those are not very standard but relatively common.
            AR      = 'ar',
            AS      = 'as',
            NM      = 'nm',
            OBJCOPY = 'objcopy',
            OBJDUMP = 'objdump',
            RANLIB  = 'ranlib',
            STRIP   = 'strip',

            PATH = cat('${jagen:bin_dir}/', value, ':$PATH')
        }
    }
}

template {
    match = { build = { type = isnot 'cmake' } },
    apply = {
        env = {
            CFLAGS = '',
            CXXFLAGS = '',
            LDFLAGS = ''
        }
    }
}

-- uses

template {
    match = { uses = bind(value, oftype 'string') },
    apply = { uses = { value } }
}

template {
    match = {
        uses = each,
        stages = { configure = some }
    },
    apply = {
        stages = {
            configure = {
                inputs = { bind(each, as_target, with_stage 'install') }
            }
        }
    }
}

-- final

template {
    final = true,
    match = {
        uses = each,
    },
    apply = {
        import = {
            [each] = from(each, 'export')
        }
    }
}

template {
    final = true,
    match = {
        build = { toolchain = value },
    },
    apply = {
        toolchain = from(value, 'export'),
    }
}

template {
    final = true,
    match = {
        toolchain = {
            cflags   = anyof { value 'cflags',   none },
            cxxflags = anyof { value 'cxxflags', none },
            ldflags  = anyof { value 'ldflags',  none },
        }
    },
    apply = {
        env = {
            jagen_pkg__cflags   = value 'cflags',
            jagen_pkg__cxxflags = value 'cxxflags',
            jagen_pkg__ldflags  = value 'ldflags',
        }
    }
}

template {
    final = true,
    match = {
        build = {
            with_install_dir = true
        },
        install = {
            dir = value 'install_dir'
        },
        toolchain = {
            cflags   = anyof { value 'cflags',   none },
            cxxflags = anyof { value 'cxxflags', none },
            ldflags  = anyof { value 'ldflags',  none }
        }
    },
    apply = {
        env = {
            jagen_pkg__cflags   = { cat('-I', value 'install_dir', '/include') },
            jagen_pkg__cxxflags = { cat('-I', value 'install_dir', '/include') },
            jagen_pkg__ldflags  = { cat('-L', value 'install_dir', '/lib') }
        }
    }
}
