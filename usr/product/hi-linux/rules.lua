-- HiSilicon Linux SDK

define_rule { 'ast-files' }

define_rule { 'hi-kernel', 'target' }

define_rule { 'hi-drivers', 'target',
    requires = { 'hi-kernel' },
}

define_rule { 'hi-sdk', 'target',
    { 'patch',
        -- to create source/kernel/linux-3.4.y symlink
        { 'hi-kernel', 'unpack' }
    },
    { 'compile',
        -- msp modules expect to find compiled kernel in source tree
        { 'hi-kernel', 'compile', 'target' }
    }
}

define_rule { 'rtl8188eu', 'target',
    requires = { 'hi-sdk' }
}

define_rule { 'cmake-modules' }

define_rule { 'hi-utils', 'target',
    requires = {
        'glib',
        'hi-sdk',
    },
    { 'configure',
        { 'cmake-modules', 'unpack'              },
    }
}

define_rule { 'karaoke-player', 'target',
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

define_rule { 'hia-astdisplayservice', 'target',
    requires = {
        'karaoke-player'
    },
    { 'configure' }
}

define_rule { 'rootfs', 'target',
    requires = {
        'busybox',
        'dropbear',
        'e2fsprogs',
        'hdparm',
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

if Jagen.flag 'debug' then
    define_rule { 'gdb', 'host' }
    define_rule { 'strace', 'target' }
end

require 'chicken'
