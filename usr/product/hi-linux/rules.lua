-- HiSilicon Linux SDK

require 'chicken'

Pkg:add { 'ast-files' }

Pkg:add { 'hi-kernel' }

Pkg:add { 'hi-sdk', 'target',
    { 'patch',
        { 'hi-kernel',    'unpack' },
    }
}

Pkg:add { 'hi-drivers', 'target',
    { 'configure',
        requires = { 'hi-sdk' }
    }
}

Pkg:add { 'rtl8188eu', 'target',
    { 'compile',
        requires = { 'hi-sdk' }
    }
}

Pkg:add { 'cmake-modules' }

Pkg:add { 'hi-utils', 'target',
    { 'configure',
        requires = { 'hi-sdk' },
        { 'cmake-modules', 'unpack'              },
    }
}

Pkg:add { 'karaoke-player', 'target',
    { 'configure',
        requires = {
            'chicken-eggs',
            'connman',
            'dbus',
            'ffmpeg',
            'hi-sdk',
            'libass',
            'libuv',
            'soundtouch',
        },
        { 'cmake-modules', 'unpack'              },
    }
}

Pkg:add { 'hia-astdisplayservice', 'target',
    { 'configure',
        requires = {
            'karaoke-player'
        }
    }
}

Pkg:add { 'rootfs', 'target',
    { 'install',
        requires = {
            'busybox',
            'dropbear',
            'hi-drivers',
            'hi-sdk',
            'hi-utils',
            'hia-astdisplayservice',
            'karaoke-player',
            'rtl8188eu',
            'wpa_supplicant',
        },
        { 'ast-files', 'unpack'           },
    },
    { 'deploy' }
}

if jagen.flag 'debug' then
    Pkg:add { 'gdb', 'host' }
    Pkg:add { 'strace', 'target' }
end
