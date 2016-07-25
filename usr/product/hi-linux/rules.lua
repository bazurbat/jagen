-- HiSilicon Linux SDK

local firmware_rule = {
    config = 'target',
    install = {
        prefix = '/usr'
    }
}

define_rule { 'cmake-modules' }

define_rule { 'ast-files' }

define_rule { 'firmware-utils', 'host' }

define_rule { 'hi-kernel', 'target' }

define_rule { 'hi-drivers', 'target',
    requires = { 'hi-kernel' },
}

define_rule { 'hi-sdk', 'target',
    template = firmware_rule,
    { 'compile',
        -- msp modules expect to find compiled kernel in source tree
        { 'hi-kernel', 'compile', 'target' }
    }
}

define_rule { 'rtl8188eu', 'target',
    requires = {
        'hi-kernel',
        'hi-sdk',
    }
}

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
    template = firmware_rule,
    requires = {
        'chicken-eggs',
        'connman',
        'dbus',
        'ffmpeg',
        'hi-sdk',
        'libass',
        'libuv',
        'soundtouch',
        'wpa_supplicant',
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
        'firmware-utils',
        'hdparm',
        'hi-drivers',
        'hi-sdk',
        'hi-utils',
        'hia-astdisplayservice',
        'rtl8188eu',
    },
    { 'install',
        { 'ast-files', 'unpack' },
    },
    { 'deploy' }
}

define_rule { 'firmware-utils', 'target',
    requires = { 'gpgme' }
}

if Jagen.flag 'debug' then
    define_rule { 'gdb', 'host' }
    define_rule { 'strace', 'target' }
end

require 'chicken'
