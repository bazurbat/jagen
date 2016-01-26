-- HiSilicon Linux SDK

package { 'make', 'host',
    source = 'make-3.81.tar.bz2'
}

package { 'hi-kernel' }

package { 'hi-sdk-tools' }

package { 'hi-sdk', 'target',
    { 'tools_install',
        { 'hi-sdk-tools', 'unpack'            },
        { 'toolchain',    'install', 'target' }
    },
    { 'prepare'           },
    { 'hiboot_install'    },
    { 'linux_install'     },
    { 'rootfs_install'    },
    { 'common_install'    },
    { 'msp_install'       },
    { 'component_install' },
    { 'rootbox_install'   },
}

-- package { 'cmake-modules' }
--
-- package { 'libuv', 'target' }
--
-- package { 'ffmpeg', 'target' }
--
-- package { 'hi-utils', 'target',
--     { 'build',
--         { 'cmake-modules', 'unpack'              },
--         { 'hi-sdk',        'component', 'target' }
--     }
-- }
--
-- package { 'karaoke-player', 'target',
--     { 'build',
--         { 'cmake-modules', 'unpack'              },
--         { 'ffmpeg',        'install',   'target' },
--         { 'hi-sdk',        'component', 'target' },
--         { 'libuv',         'install',   'target' },
--     }
-- }
