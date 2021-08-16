-- template {
--     parse = true,
--     match = { value 'name', anyof(value 'config', none) },
--     apply = {
--         none, none,
--         name   = value 'name',
--         config = value 'config'
--     }
-- }
--
-- template {
--     parse = true,
--     match = {
--         name = value 'name'
--     },
--     apply = {
--         ref = value 'name'
--     }
-- }
--
-- template {
--     parse = true,
--     match = {
--         name   = value 'name',
--         config = value 'config'
--     },
--     apply = {
--         ref = cat(value 'name', ':', value 'config')
--     }
-- }

template {
    parse = true,
    match = { build = bind(value(), oftype 'string') },
    apply = { build = { type = value() } }
}

template {
    parse = true,
    match = { build = { bind(value(), oftype 'string') } },
    apply = { build = { none, type = value() } }
}

template {
    parse = true,
    match = { install = bind(value(), oftype 'string') },
    apply = { install = { type = value() } }
}

template {
    parse = true,
    match = { install = { bind(value(), oftype 'string') } },
    apply = { install = { none, type = value() } }
}

template {
    match = { config = none },
    apply = {
        work_dir = '${jagen.build_dir}/${name}',
    }
}

template {
    match = { config = some },
    apply = {
        work_dir = '${jagen.build_dir}/${name}:${config}',
    }
}

template {
    match = { build = some },
    apply = {
        build = {
            dir = '${work_dir}'
        }
    }
}

template {
    match = { source = some },
    apply = {
        stages = {
            clean = {}
        }
    }
}

template {
    match = { source = { type = anyof('git', 'hg') } },
    apply = {
        source = { scm = true }
    }
}

template {
    match = { source = { type = anyof('git', 'hg') } },
    apply = {
        stages = {
            update = { inputs = { stage 'clean' } }
        }
    }
}

template {
    match = { source = { type = '^dist' } },
    apply = {
        stages = {
            unpack = { inputs = { stage 'clean' } }
        }
    }
}

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

template {
    match = {
        build = { system = value 'system', arch = none }
    },
    apply = {
        build = {
            arch = bind(value 'system', match('^(%w+)-?'))
        }
    }
}

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

template {
    match = { build = { type = some } },
    apply = {
        stages = {
            compile = { inputs = { stage 'configure' } }
        }
    }
}

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

template {
    match = {
        install = { type = some },
        stages = {
            compile = some
        }
    },
    apply = {
        stages = {
            install = { inputs = { stage 'compile' } }
        }
    }
}

template {
    match = { uses = bind(value 'uses', oftype 'string') },
    apply = { uses = { value 'uses' } }
}

template {
    match = {
        uses = each,
        stages = { configure = some }
    },
    apply = {
        stages = {
            configure = {
                inputs = { bind(each, as_target, with_stage('install')) }
            }
        }
    }
}

template {
    final = true,
    match = {
        uses = each,
    },
    apply = {
        import = {
            [each] = pkg(each, 'export')
        }
    }
}

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
    apply = {
        uses = { '${build.toolchain}' }
    }
}

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
    match = { install = { root = some, prefix = some } },
    apply = {
        install = { dir = '${install.root}${install.prefix}' }
    }
}

template {
    match = { config = 'target', build = { type = 'cmake' } },
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
    match = { source = { type = anyof('git', 'hg') } },
    apply = {
        source = { dir = '${jagen.source_dir}/${name}' }
    }
}

-- template {
--     match = {
--         build = { in_source = true },
--         source = { type = 'git' }
--     },
--     source = {
--         ignore_dirty = 'in_source'
--     }
-- }

-- template {
--     match = { build = { type = 'android-gradle' } },
--     source = {
--         ignore_dirty = false
--     },
--     build = {
--         in_source = true,
--         toolchain = 'android-sdk-tools:host',
--         profile   = 'debug',
--         clean     = '$build.dir/app/build'
--     }
-- }

-- template {
--     match = { build = { in_source = true } },
--     build = { dir = '${source.dir}' }
-- }

-- template {
--     name = 'kernel',
--     config = '${config}',
--     match = {
--         build = { type = 'linux-module' }
--     }
-- }

-- template {
--     match = { build = { type = 'linux-module' } },
--     apply = {
--         stages = {
--             configure = { inputs = { target('kernel', 'configure') } },
--             compile   = { inputs = { target('kernel', 'compile') } },
--             install   = { inputs = { target('kernel', 'install') } },
--         }
--     }
-- }

template {
    match = { source = { dir = some } },
    apply = {
        export = {
            dir = '${source.dir}',
            source = { dir = '${source.dir}' },
        }
    }
}

template {
    match = { build = { dir = some } },
    apply = {
        export = {
            build = {
                dir = '${build.dir}'
            }
        }
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

template {
    match = {
        build = { cflags = some, cxxflags = none }
    },
    apply = {
        build = {
            cxxflags = '${build.cflags}'
        }
    }
}
