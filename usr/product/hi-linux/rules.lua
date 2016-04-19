-- HiSilicon Linux SDK

rule { 'ast-files' }

rule { 'hi-kernel' }

rule { 'hi-sdk', 'target',
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

rule { 'hi-drivers', 'target',
    { 'configure',
        { 'hi-sdk', 'linux', 'target' }
    }
}

rule { 'rtl8188eu', 'target',
    { 'compile',
        { 'hi-sdk', 'linux', 'target' }
    }
}

rule { 'cmake-modules' }

rule { 'hi-utils', 'target',
    { 'configure',
        { 'cmake-modules', 'unpack'              },
        { 'hi-sdk',        'component', 'target' }
    }
}

rule { 'karaoke-player', 'target',
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

rule { 'rootfs', 'target',
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
    rule { 'strace', 'target' }
end
