-- HiSilicon Linux SDK

package { 'make', 'host',
    source = 'make-3.81.tar.bz2'
}

package { 'hi-kernel' }

package { 'hi-sdk', 'target',
    { 'tools'     },
    { 'prepare',
        { 'hi-kernel', 'unpack' }
    },
    { 'hiboot'    },
    { 'linux'     },
    { 'rootfs'    },
    { 'common'    },
    { 'msp'       },
    { 'component' }
}

package { 'cmake-modules' }

package { 'libuv', 'target' }

package { 'ffmpeg', 'target' }

package { 'hi-utils', 'target',
    { 'build',
        { 'cmake-modules', 'unpack'                      },
        { 'hi-sdk',        'install_component', 'target' }
    }
}

package { 'karaoke-player', 'target',
    { 'build',
        { 'cmake-modules', 'unpack'                      },
        { 'ffmpeg',        'install',           'target' },
        { 'hi-sdk',        'install_component', 'target' },
        { 'libuv',         'install',           'target' },
    }
}
