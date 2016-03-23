-- HiSilicon Linux SDK

package { 'make', 'host',
    source = 'make-3.81.tar.bz2'
}

package { 'ast-files' }

package { 'hi-sdk', 'target',
    { 'tools',
        { 'hi-sdk-tools', 'unpack' }
    },
    { 'prepare',
        requires = {
            'toolchain',
            { 'make', 'host' },
        },
        { 'hi-kernel', 'unpack' },
    },
    { 'hiboot'    },
    { 'linux'     },
    { 'rootfs'    },
    { 'common'    },
    { 'msp'       },
    { 'component' },
    { 'mkload'    },
}

package { 'hi-sdk-tools',
    { 'unpack', { 'hi-sdk', 'unpack' } }
}

package { 'hi-kernel',
    { 'unpack', { 'hi-sdk', 'unpack' } }
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
    { 'prepare',
        { 'ast-files', 'unpack'           },
        { 'hi-sdk',    'mkload', 'target' }
    },
    { 'hi-utils',
        requires = { 'hi-utils' }
    },
    { 'dropbear',
        requires = { 'dropbear' }
    },
    { 'dropbear-key' },
    { 'install',
        requires = {
            'hi-drivers',
            'karaoke-player',
        }
    },
}

if jagen.flag 'debug' then
    package { 'strace', 'target' }
end
