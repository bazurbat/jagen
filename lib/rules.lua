config {
    name = 'jagen',
    dir = {
        bin     = '${dir.root}/bin',
        build   = '${dir.root}/build',
        include = '${dir.root}/include',
        log     = '${dir.root}/log',
        src     = '${dir.root}/src',
    },
    dist_dir = 'dist',
    src_dir = 'src',
    flags = {},
    source_exclude = {},
    build_profile = "release"
}

config {
    name = 'root',
    dir = os.getenv('jagen_root_dir'),
    bin_dir = '${dir}/bin',
    src_dir = '${dir}/src',
    dist_dir = '${dir}/dist',
    build_dir = '${dir}/build',
    include_dir = '${dir}/include',
    log_dir = '${dir}/log',
}

config {
    name = 'ccache',
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
    match = { as 'name', anyof(as 'config', none) },
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
    match = { source = { type = anyof('git', 'hg') } },
    apply = { source = { scm = true } }
}

template {
    match = { source = { name = anyof(value, none) } },
    apply = { source = { name = anyof(value, '${name}') } }
}

template {
    match = {
        source = {
            location = value 'location',
            filename = anyof(value 'filename', none)
        }
    },
    apply = {
        source = {
            filename = anyof(
                value 'filename',
                bind(value 'location', match '^.*/(.+)$'),
                value 'location'),
            basename = bind(value 'location',
                            anyof(match '^.*/(.+)%.tar%.%w+$',
                                  match '^.*/(.+)%.[^.]+$',
                                  match '^.*/(.+)$',
                                  match '.*'))
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
            dir = cat('${root:src_dir}', '/', value 'name')
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
            dir = cat('${root:build_dir}', '/', value 'name')
        }
    }
}

template {
    match = { source = { dir = some } },
    apply = {
        export = {
            source = { dir = '${source.dir}' },
        }
    }
}

-- work_dir

template {
    match = { config = none },
    apply = {
        work_dir = '${root:build_dir}/${name}',
    }
}

template {
    match = { config = some },
    apply = {
        work_dir = '${root:build_dir}/${name}:${config}',
    }
}

-- clean

template {
    match = { source = some },
    apply = { stages = { clean = {} } }
}

-- update

template {
    match = { source = { type = anyof('git', 'hg') } },
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

template {
    match = {
        build = { cflags = some, cxxflags = none }
    },
    apply = {
        build = { cxxflags = '${build.cflags}' }
    }
}

-- template {
--     match = { install = { type = 'linux-kernel' } },
--     apply = {
--         -- TODO: if not in source
--         KBUILD_OUTPUT = "${build.dir}"
--     }
-- }

template {
    final = true,
    match = {
        build = { toolchain = value }
    },
    apply = {
        build = {
            system = from(value, 'export.system'),
            arch   = from(value, 'export.arch'),
            cpu    = from(value, 'export.cpu'),
        }
    }
}

template {
    final = true,
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
    final = true,
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
    final = true,
    match = {
        build = {
            type = 'cmake',
            toolchain = as 'toolchain',
            cmake_executable = anyof(value 'cmake_executable', none)
        }
    },
    apply = {
        build = {
            cmake_executable = anyof(
                value 'cmake_executable',
                from(value 'toolchain', 'export.cmake_executable'),
                'cmake'
            )
            -- cmake_generator      = 'g',
            -- cmake_options        = 'o',
            -- cmake_module_path    = 'm',
            -- cmake_toolchain_file = 't'
        }
    }
}

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
        build   = { type = some },
        install = anyof(none, { type = none })
    },
    apply = {
        install = {
            type = '${build.type}'
        }
    }
}

-- stage: install

template {
    match = { install = { type = some } },
    apply = { stages  = { install = {} } }
}

template {
    match = {
        build   = anyof(none, { type = none }),
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
        build   = anyof(none, { type = none }),
        install = { type = some },
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
            prefix = '${jagen.host_dir}',
            root = ''
        },
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
}

template {
    match = {
        class = contains 'target',
        build = { type = 'cmake' }
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
    match = { install = { dir = some } },
    apply = {
        export = {
            install = {
                dir = '${install.dir}'
            }
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
    match = { build = { toolchain = some } },
    apply = { uses = { '${build.toolchain}' } }
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
        import = {
            toolchain = from(value, 'export'),
        }
    }
}