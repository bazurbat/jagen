package { 'jagen',
    abstract    = true,
    dir         = os.getenv('jagen_dir'),
    root_dir    = os.getenv('jagen_root_dir'),
    build_dir   = os.getenv('jagen_build_dir'),
    include_dir = os.getenv('jagen_include_dir'),
    bin_dir     = '${build_dir}/bin',
    dist_dir    = '${root_dir}/dist',
    lib_dir     = '${root_dir}/lib',
    log_dir     = '${root_dir}/log',
    src_dir     = '${root_dir}/src',
    build_file         = '${build_dir}/build.ninja',
    build_targets_file = '${build_dir}/.build-targets',
    build_args_file    = '${build_dir}/.build-args',
    self = {
        bin_dir = '${dir}/bin',
        src_dir = '${dir}/src',
        cmd     = '${self.src_dir}/cmd.sh',
    },
    export = {
        bin_dir     = '${bin_dir}',
        build_dir   = '${build_dir}',
        dir         = '${dir}',
        dist_dir    = '${dist_dir}',
        include_dir = '${include_dir}',
        log_dir     = '${log_dir}',
        root_dir    = '${root_dir}',
        src_dir     = '${src_dir}',
        build_args_file = '${build_args_file}',
        env = {
            -- Disable passphrase querying.
            GIT_SSH_COMMAND = 'ssh -o BatchMode=yes',
            -- Do not prompt on the terminal (e.g. when asking for HTTP credentials).
            GIT_TERMINAL_PROMPT = 0,
            -- Never install the translations.
            LINGUAS = '<unset>',
            -- Do not run tools with an interactive graphical UI.
            DISPLAY = '<unset>'
        }
    },
    cmake = {
    }
}

-- package { 'ccache',
--     env = {
--         jagen_ccache="ccache",
--
--         -- This should allow sharing cache between different roots.
--         CCACHE_BASEDIR="$HOME",
--
--         -- The default check uses compiler's mtime and size, which can cause
--         -- false misses due to autogenerated wrappers for some SDKs.
--         CCACHE_COMPILERCHECK="content",
--
--         -- Performance improvements (read the manual).
--         CCACHE_SLOPPINESS="file_stat_matches,include_file_mtime,time_macros"
--     }
-- }

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
    match = {
        name   = { value 'name' },
        source = { name = anyof { value, none } }
    },
    apply = {
        source = { name = anyof { value, value 'name' } }
    }
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
                bind { value 'location', match '^.*/(.+)$' },
                value 'location' },
            basename = bind {
                value 'location',
                anyof {
                    match '^.*/(.+)%.tar%.%w+$',
                    match '^.*/(.+)%.[^.]+$',
                    match '^.*/(.+)$',
                    match '.*'
                }
            }
        }
    }
}

template {
    match = anyof {
        { source = none, build = some },
        { source = { type = none }, build = some }
    },
    apply = {
        source = { type = 'dir' }
    }
}

template {
    match = {
        name = value 'name',
        source = {
            type = 'dir',
            location = optional(value)
        }
    },
    apply = {
        source = {
            dir = anyof {
                value,
                path { from('jagen', 'src_dir'), value 'name' }
            }
        }
    }
}

template {
    match = {
        name = value 'name',
        source = {
            dir = none,
            scm = true
        }
    },
    apply = {
        source = {
            dir = path { from('jagen', 'src_dir'), value 'name' }
        }
    }
}

template {
    match = {
        source = {
            dir = none,
            scm = false
        }
    },
    apply = {
        source = {
            dir = path { from('jagen', 'build_dir'), value 'name' }
        }
    }
}

-- work_dir

template {
    match = {
        name   = value,
        config = none
    },
    apply = {
        work_dir = path { from('jagen', 'build_dir'), value }
    }
}

template {
    match = {
        name   = value 'name',
        config = value 'config'
    },
    apply = {
        work_dir = path { from('jagen', 'build_dir'),
                          join(':', { value 'name', value 'config' }) }
    }
}

-- stage: clean

template {
    match = {
        source = some
    },
    apply = {
        stage = { clean = {} }
    }
}

-- update

template {
    match = { source = { scm = true } },
    apply = {
        stage = {
            update = {
                inputs = { stage 'clean' },
                work_dir = from('jagen', 'build_dir')
            }
        }
    }
}

-- unpack

template {
    match = { source = { type = '^dist' } },
    apply = {
        stage = {
            unpack = {
                inputs = { stage 'clean' },
                work_dir = from('jagen', 'build_dir')
            }
        }
    }
}

-- patch

template {
    match = {
        patches = some,
        source  = { dir = value 'source.dir' },
    },
    apply = {
        stage = {
            patch = {
                inputs = {
                    from('<self>', anyof {
                            stage 'update',
                            stage 'unpack',
                            stage 'clean'
                        })
                },
                work_dir = value 'source.dir'
            }
        }
    }
}

-- build

template {
    match = {
        source = { dir = value      },
        build  = { in_source = true },
    },
    apply = {
        build = { dir = value }
    }
}

template {
    match = {
        work_dir = value,
        build = { dir = none  }
    },
    apply = {
        build = { dir = value }
    }
}

template {
    match = {
        build = {
            profile = none
        }
    },
    apply = {
        build = {
            profile = 'release'
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
        build = {
            type = some,
            dir  = some
        }
    },
    apply = {
        stage = {
            configure = {
                inputs = {
                    anyof {
                        stage 'patch',
                        stage 'update',
                        stage 'unpack',
                        stage 'clean'
                    }
                },
                work_dir = expand '${build.dir}'
            } 
        }
    }
}

-- stage: compile

template {
    match = {
        build = {
            type = some,
            dir  = some
        } 
    },
    apply = {
        stage = {
            compile = {
                inputs = {
                    stage 'configure'
                },
                work_dir = expand '${build.dir}'
            }
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
    match = {
        build   = { dir = value 'build.dir' },
        install = { type = some }
    },
    apply = {
        stage = {
            install = {
                work_dir = value 'build.dir'
            }
        }
    }
}

template {
    match = {
        build   = anyof { none, { type = none } },
        install = { type = some },
        stage  = { unpack = some }
    },
    apply = {
        stage = {
            install = { inputs = { stage 'unpack' } }
        }
    }
}

template {
    match = {
        build   = anyof { none, { type = none } },
        install = { type  = some },
        stage  = { clean = some }
    },
    apply = {
        stage = {
            install = { inputs = { stage 'clean' } }
        }
    }
}

template {
    match = {
        install = { type    = some },
        stage  = { compile = some }
    },
    apply = {
        stage = {
            install = { inputs = { stage 'compile' } }
        }
    }
}

-- default install sysroots

template {
    match = { class = contains 'host' },
    apply = {
        install = {
            prefix = path{from('jagen', 'root_dir'), 'host'},
            root   = ''
        }
    }
}

template {
    match = { class = contains 'target' },
    apply = {
        install = {
            prefix = '',
            root   = path{from('jagen', 'root_dir'), 'target'}
        }
    }
}

-- pkgconfig

template {
    match = {
        install = {
            prefix = value,
            root   = none
        }
    },
    apply = {
        env = {
            PKG_CONFIG_PATH = cat { value, '/lib/pkgconfig' }
        }
    }
}

template {
    match = {
        install = {
            root = value
        }
    },
    apply = {
        env = {
            PKG_CONFIG_SYSROOT_DIR  = value,
            PKG_CONFIG_LIBDIR = cat { value, '/lib/pkgconfig' },
            PKG_CONFIG_PATH   = cat { value, '/usr/lib/pkgconfig' },

            -- pkg-config tries to be smart and removes -I and -L flags from
            -- it's output when they resemble system paths. This causes
            -- SYSROOT_DIR to not be added to them, which prevents packages to
            -- find each other when building the sysroot itself.
            PKG_CONFIG_ALLOW_SYSTEM_CFLAGS = 1,
            PKG_CONFIG_ALLOW_SYSTEM_LIBS = 1
        }
    }
}

-- kbuild

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
    match = {
        install = {
            root   = value 'root',
            prefix = value 'prefix' 
        }
    },
    apply = {
        install = { dir = cat { value 'root', value 'prefix' } }
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
            toolchain = 'system-native-gcc'
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
        class = contains(match 'toolchain'),
        export = {
            system = value,
            arch   = none
        }
    },
    apply = {
        export = {
            arch = bind { value, match('^(%w+)-?') }
        }
    }
}

template {
    match = {
        class = contains(match 'toolchain')
    },
    apply = {
        export = {
            cc  = default 'gcc',
            cxx = default 'g++',
            cpp = default 'cpp',
            ld  = default 'ld',
        }
    }
}

template {
    match = {
        class = contains(match 'toolchain%-gcc')
    },
    apply = {
        export = {
            cc  = 'gcc',
            cxx = 'g++',
        }
    }
}

template {
    match = {
        class = contains(match 'toolchain%-clang')
    },
    apply = {
        export = {
            cc  = 'clang',
            cxx = 'clang++',
        }
    }
}

template {
    match = {
        class = contains(match 'toolchain'),
        export = {
            system = optional(value 'system'),
            cc     = value 'cc',
            cxx    = value 'cxx',
            cpp    = value 'cpp',
            ld     = value 'ld'
        }
    },
    apply = {
        export = {
            prefix  = nonempty(join('-', { value 'system', '' })),
            cc      = join('-', { value 'system', value 'cc'  }),
            cxx     = join('-', { value 'system', value 'cxx' }),
            cpp     = join('-', { value 'system', value 'cpp' }),
            ld      = join('-', { value 'system', value 'ld'  }),
            -- Some of those are not very standard but relatively common.
            -- AR      = join('-', { value, 'ar' }),
            -- AS      = join('-', { value, 'as' }),
            -- NM      = join('-', { value, 'nm' }),
            -- OBJCOPY = join('-', { value, 'objcopy' }),
            -- OBJDUMP = join('-', { value, 'objdump' }),
            -- RANLIB  = join('-', { value, 'ranlib'  }),
            -- STRIP   = join('-', { value, 'strip'   }),
        }
    }
}

template {
    match = {
        class = contains(match 'cross%-toolchain')
    },
    apply = {
        export = {
            cmake = {
                options = {
                    '-DCMAKE_SYSTEM_NAME=Linux'
                }
            }
        }
    }
}

-- cmake

template {
    match = {
        build = {
            type    = 'cmake',
            profile = anyof { 'release', none }
        }
    },
    apply = {
        build = {
            cmake = {
                build_type = 'Release',
                config = 'RELEASE'
            }
        }
    }
}

template {
    match = {
        build = {
            type    = 'cmake',
            profile = 'debug'
        }
    },
    apply = {
        build = {
            cmake = {
                build_type = 'Debug',
                config = 'DEBUG'
            }
        }
    }
}

template {
    match = {
        build = {
            type    = 'cmake',
            profile = 'release_with_debug'
        }
    },
    apply = {
        build = {
            cmake = {
                build_type = 'RelWithDebInfo',
                config = 'RELWITHDEBINFO'
            }
        }
    }
}

-- dir export

template {
    match = {
        source = { dir = optional(value 'source.dir') },
        build  = { dir = optional(value 'build.dir')  }

    },
    apply = {
        export = {
            dir    = {       value 'source.dir' },
            source = { dir = value 'source.dir' },
            build  = { dir = value 'build.dir'  }
        }
    }
}

-- uses

template {
    match = {
        uses = each,
        stage = { configure = some }
    },
    apply = {
        stage = {
            configure = {
                inputs = {
                    from(each, anyof { stage 'install', stage 'compile' })
                }
            }
        }
    }
}

template {
    match = {
        build = { toolchain = value },
    },
    apply = {
        toolchain = from(value, 'export'),
    }
}

template {
    match = {
        build = {
            with_install_dir = true
        },
        install = { dir = value }
    },
    apply = {
        toolchain = {
            cflags   = { cat { '-I', value, '/include' } },
            cxxflags = { cat { '-I', value, '/include' } },
            ldflags  = { cat { '-L', value, '/lib'     } }
        }
    }
}

template {
    match = {
        build = {
            cc   = optional(value 'build.cc'),
            cxx  = optional(value 'build.cxx'),
            cpp  = optional(value 'build.cpp'),
            ld   = optional(value 'build.ld'),
            path = optional(value 'build.path'),
            toolchain = value
        },
        toolchain = {
            cc   = optional(value 'toolchain.cc'),
            cxx  = optional(value 'toolchain.cxx'),
            cpp  = optional(value 'toolchain.cpp'),
            ld   = optional(value 'toolchain.ld'),
            path = optional(value 'toolchain.path')
        }
    },
    apply = {
        env = {
            -- CMake honors CC and CXX but not LD. It uses compiler for linking.
            CC  = optional(anyof(value 'build.cc',  value 'toolchain.cc')),
            CXX = optional(anyof(value 'build.cxx', value 'toolchain.cxx')),
            CPP = optional(anyof(value 'build.cpp', value 'toolchain.cpp')),
            LD  = optional(anyof(value 'build.ld',  value 'toolchain.ld')),
            PATH = join(':', {
                                 value 'build.path',
                                 path{from('jagen', 'bin_dir'), value},
                                 value 'toolchain.path',
                                 os.getenv('PATH')
            })
        }
    }
}

template {
    match = {
        build = {
            system = optional(value 'system'),
            arch   = optional(value 'arch'),
            cpu    = optional(value 'cpu')
        },
        toolchain = {
            system = optional(value 'toolchain.system'),
            arch   = optional(value 'toolchain.arch'),
            cpu    = optional(value 'toolchain.cpu')
        }
    },
    apply = {
        build = {
            system = anyof { value 'system', value 'toolchain.system' },
            arch   = anyof { value 'arch',   value 'toolchain.arch'   },
            cpu    = anyof { value 'cpu',    value 'toolchain.cpu'    }
        }
    }
}

template {
    match = {
        build = {
            type     = isnot 'cmake',
            cflags   = optional(value 'cflags'),
            cxxflags = optional(value 'cxxflags'),
            ldflags  = optional(value 'ldflags')
        },
        toolchain = {
            cflags   = optional(value 'toolchain_cflags'),
            cxxflags = optional(value 'toolchain_cxxflags'),
            ldflags  = optional(value 'toolchain_ldflags')
        }
    },
    apply = {
        env = {
            CFLAGS   = nonempty(join { value 'cflags',   value 'toolchain_cflags'   }),
            CXXFLAGS = nonempty(join { value 'cxxflags', value 'toolchain_cxxflags' }),
            LDFLAGS  = nonempty(join { value 'ldflags',  value 'toolchain_ldflags'  })
        }
    }
}

template {
    match = {
        build = {
            type = 'cmake',
            cmake = {
                executable = optional(value 'executable'),
                generator  = optional(value 'generator'),
            }
        },
        toolchain = {
            cmake = optional {
                executable = optional(value 'toolchain_executable'),
                generator  = optional(value 'toolchain_generator'),
            }
        }
    },
    apply = {
        build = {
            cmake = {
                executable = anyof {
                    value 'executable',
                    value 'toolchain_executable',
                    'cmake'
                },
                generator = anyof {
                    value 'generator',
                    value 'toolchain_generator',
                    'Ninja'
                }
            }
        }
    }
}

template {
    match = {
        build = {
            cmake = {
                toolchain_file = optional(value 'from_package')
            }
        },
        toolchain = {
            cmake = optional {
                toolchain_file = optional(value 'from_toolchain')
            }
        }
    },
    apply = {
        build = {
            cmake = {
                template_options = {
                    argument('-DCMAKE_TOOLCHAIN_FILE',
                        anyof { value 'from_package',
                                value 'from_toolchain'  })
                }
            }
        }
    }
}

template {
    match = {
        build = { type = 'cmake' },
        toolchain = {
            cmake = optional {
                options = optional(value)
            }
        }
    },
    apply = {
        build = {
            cmake = {
                template_options = {
                    value
                }
            }
        }
    }
}

template {
    match = {
        build = {
            cmake = {
                module_path = optional(value 'from_package')
            }
        },
        toolchain = {
            cmake = optional {
                module_path = optional(value 'from_toolchain')
            }
        }
    },
    apply = {
        build = {
            cmake = {
                template_options = {
                    argument('-DCMAKE_MODULE_PATH', join(';', { value 'from_package', value 'from_toolchain'  }))
                }
            }
        }
    }
}

template {
    match = {
        build = {
            cmake = {
                build_type = value
            }
        }
    },
    apply = {
        build = {
            cmake = {
                template_options = {
                    argument('-DCMAKE_BUILD_TYPE', value)
                }
            }
        }
    }
}

template {
    match = {
        build = {
            type     = 'cmake',
            cflags   = optional(value 'cflags'),
            cxxflags = optional(value 'cxxflags'),
            ldflags  = optional(value 'ldflags'),
            cmake    = { config = value }
        }
    },
    apply = {
        build = {
            cmake = {
                template_options = {
                    argument(cat{'-DCMAKE_C_FLAGS_', value },  join { value 'cflags'   }),
                    argument(cat{'-DCMAKE_CXX_FLAGS_', value}, join { value 'cxxflags' }),
                    argument(cat{'-DCMAKE_EXE_LINKER_FLAGS_', value}, join { value 'ldflags' }),
                }
            }
        }
    }
}

template {
    match = {
        build   = { type = 'cmake' },
        install = { root = value   }
    },
    apply = {
        build = {
            cmake = {
                template_options = {
                    argument('-DCMAKE_FIND_ROOT_PATH', value),
                    '-DCMAKE_FIND_ROOT_PATH_MODE_PROGRAM=NEVER',
                    '-DCMAKE_FIND_ROOT_PATH_MODE_LIBRARY=ONLY',
                    '-DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE=ONLY',
                    '-DCMAKE_FIND_ROOT_PATH_MODE_PACKAGE=ONLY',
                }
            }
        }
    }
}

template {
    match = {
        build   = { type = 'cmake' },
        install = { prefix = value }
    },
    apply = {
        build = {
            cmake = {
                template_options = {
                    argument('-DCMAKE_INSTALL_PREFIX', value)
                }
            }
        }
    }
}

template {
    match = {
        build = { type = 'cmake' },
        -- CMake version >= 3.1
        from('jagen', 'cmake.supports_disable_package_registry'),
    },
    apply = {
        build = {
            cmake = {
                template_options = {
                    '-DCMAKE_EXPORT_NO_PACKAGE_REGISTRY=YES',
                    '-DCMAKE_FIND_PACKAGE_NO_PACKAGE_REGISTRY=YES',
                    '-DCMAKE_FIND_PACKAGE_NO_SYSTEM_PACKAGE_REGISTRY=YES'
                }
            }
        }
    }
}

template {
    match = {
        build = { type = 'cmake' },
        -- CMake version >= 3.5
        from('jagen', 'cmake.supports_export_compile_commands'),
    },
    apply = {
        build = {
            cmake = {
                template_options = {
                    '-DCMAKE_EXPORT_COMPILE_COMMANDS=YES'
                }
            }
        }
    }
}

template {
    match = {
        build = {
            cmake = { template_options = value }
        }
    },
    apply = {
        build = {
            options = { value }
        }
    }
}

template {
    match = {
        source = { dir = value 'source.dir' },
        build  = {
            options = value 'options',
            cmake = {
                executable = value 'cmake',
                generator  = value 'generator'
            }
        }
    },
    apply = {
        build = {
            command = {
                configure = {
                    value 'cmake', '--no-warn-unused-cli',
                    join {'-G', quoted(value 'generator')},
                    value 'options',
                    value 'source.dir'
                }
            }
        }
    }
}

template {
    match = {
        source = { dir = some },
        build  = {
            type = 'make',
            in_source = true
        }
    },
    apply = {
        stage = {
            clean = {
                work_dir = expand '${source.dir}',
                command = { 'make', 'clean' }
            }
        }
    }
}
