-- HiSilicon Linux SDK

package { 'make', 'host',
    source = 'make-3.81.tar.bz2'
}

package { 'hi-kernel' }

package { 'hi-sdk', 'target',
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
            'libuv',
            'soundtouch',
        },
        { 'cmake-modules', 'unpack'              },
        { 'hi-sdk',        'component', 'target' },
    }
}

package { 'rootfs', 'target',
    { 'prepare',
        { 'hi-sdk', 'rootfs', 'target' }
    },
    { 'hi-utils',
        requires = { 'hi-utils' }
    },
    { 'dropbear',
        requires = { 'dropbear' }
    },
    { 'dropbear-key' },
    { 'install',
        requires = { 'karaoke-player' }
    },
}
