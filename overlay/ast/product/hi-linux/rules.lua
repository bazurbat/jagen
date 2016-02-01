-- HiSilicon Linux SDK

package { 'make', 'host',
    source = 'make-3.81.tar.bz2'
}

package { 'hi-kernel' }

package { 'hi-sdk-tools' }

package { 'hi-sdk', 'target',
    { 'tools',
        { 'make',         'install', 'host'   },
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

package { 'hi-drivers', 'target',
    { 'build',
        { 'hi-sdk', 'linux', 'target' }
    }
}

package { 'cmake-modules' }

package { 'hi-utils', 'target',
    { 'build',
        { 'cmake-modules', 'unpack'              },
        { 'hi-sdk',        'component', 'target' }
    }
}

package { 'karaoke-player', 'target',
    { 'build',
        requires = {
            'chicken-eggs',
            'ffmpeg',
            'libuv',
        },
        { 'cmake-modules', 'unpack'              },
        { 'hi-sdk',        'component', 'target' },
    }
}

package { 'rootfs', 'target',
    { 'install',
        requires = {
            'hi-utils',
            'karaoke-player',
        },
        { 'hi-sdk', 'rootbox', 'target' }
    }
}
