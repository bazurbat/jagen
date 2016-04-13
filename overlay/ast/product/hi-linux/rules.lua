-- HiSilicon Linux SDK

package { 'ast-files' }

package { 'hi-kernel' }

package { 'hi-sdk', 'target',
    { 'patch',
        { 'hi-kernel',    'unpack' },
    },
    { 'prepare', requires = { 'toolchain' } },
    { 'hiboot'    },
    { 'linux'     },
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

package { 'rtl8188eu', 'target',
    { 'compile',
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
            'connman',
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
            'dropbear',
            'hi-drivers',
            'hi-utils',
            'karaoke-player',
            'rtl8188eu',
        },
        { 'ast-files', 'unpack'           },
        { 'hi-sdk',    'mkload', 'target' }
    },
}

if jagen.flag 'debug' then
    package { 'strace', 'target' }
end
