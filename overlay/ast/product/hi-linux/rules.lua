-- HiSilicon Linux SDK

package { 'make', 'host',
    source = 'make-3.81.tar.bz2'
}

package { 'hi-kernel' }

package { 'hi-sdk-tools' }

package { 'hi-sdk', 'target',
    { 'tools',
        { 'hi-sdk-tools', 'unpack'            },
        { 'toolchain',    'install', 'target' }
    },
    { 'prepare'   },
    { 'hiboot'    },
    { 'linux'     },
    { 'rootfs'    },
    { 'common'    },
    { 'msp'       },
    { 'component' },
    { 'rootbox'   },
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
