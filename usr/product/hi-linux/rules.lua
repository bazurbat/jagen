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
    requires = { 'hi-sdk' },
    { 'configure' }
}

Pkg:add { 'rtl8188eu', 'target',
    requires = { 'hi-sdk' },
    { 'compile' }
}

Pkg:add { 'cmake-modules' }

Pkg:add { 'hi-utils', 'target',
    requires = { 'hi-sdk' },
    { 'configure',
        { 'cmake-modules', 'unpack'              },
    }
}

Pkg:add { 'karaoke-player', 'target',
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
    { 'configure',
        { 'cmake-modules', 'unpack'              },
    }
}

Pkg:add { 'hia-astdisplayservice', 'target',
    requires = {
        'karaoke-player'
    },
    { 'configure' }
}

Pkg:add { 'rootfs', 'target',
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
    { 'install',
        { 'ast-files', 'unpack' },
    },
    { 'deploy' }
}

if jagen.flag 'debug' then
    Pkg:add { 'gdb', 'host' }
    Pkg:add { 'strace', 'target' }
end
