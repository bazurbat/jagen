-- HiSilicon Linux SDK

package { 'make', 'host',
    source = 'make-3.81.tar.bz2'
}

package { 'ast-files' }

package { 'hi-kernel' }

package { 'hi-sdk-tools' }

package { 'hi-sdk', 'target',
    { 'patch',
        { 'hi-kernel',    'unpack' },
        { 'hi-sdk-tools', 'unpack' },
    },
    -- { 'tools' },
    { 'prepare',
        requires = {
            'toolchain',
            { 'make', 'host' },
        },
    },
    -- { 'hiboot'    },
    { 'linux'     },
    -- { 'rootfs'    },
    { 'common'    },
    { 'msp'       },
    { 'component' },
    { 'mkload'    },
}

package { 'hi-drivers', 'target',
    { 'configure',
        { 'hi-sdk', 'linux', 'target' }
    }
}

package { 'cmake-modules' }

package { 'hi-utils', 'target',
    { 'configure',
        { 'cmake-modules', 'unpack'              },
        { 'hi-sdk',        'component', 'target' }
    }
}

package { 'karaoke-player', 'target',
    { 'configure',
        requires = {
            'chicken-eggs',
            'ffmpeg',
            'libass',
            'libuv',
            'soundtouch',
        },
        { 'cmake-modules', 'unpack'              },
        { 'hi-sdk',        'component', 'target' },
    }
}

package { 'rootfs', 'target',
    { 'install',
        requires = {
            'busybox',
            'connman',
            'dropbear',
            'hi-drivers',
            'hi-utils',
            'karaoke-player',
        },
        { 'ast-files', 'unpack'           },
        { 'hi-sdk',    'mkload', 'target' }
    },
}

if jagen.flag 'debug' then
    package { 'strace', 'target' }
end
