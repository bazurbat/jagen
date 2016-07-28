-- HiSilicon Linux SDK

define_rule { 'initramfs', 'target',
    { 'install',
        requires = {
            'busybox',
            'hi-kernel',
            'loop-aes',
            'util-linux',
        }
    }
}

define_rule { 'loop-aes', 'target',
    { 'compile',
        { 'hi-kernel', 'configure', 'target' }
    }
}

define_rule { 'hi-sdk', 'target',
    { 'compile',
        { 'hi-kernel', 'configure', 'target' }
    }
}

define_rule { 'rootfs', 'target',
    requires = {
        'ast-files',
        'busybox',
        'dropbear',
        'e2fsprogs',
        'firmware-utils',
        'gnupg',
        'hdparm',
        'hi-drivers',
        'hi-sdk',
        'hi-utils',
        'hia-astdisplayservice',
        'rtl8188eu',
    },
    { 'deploy' }
}

-- explicit definition of firmware utils to avoid building gpgme for host

define_rule { 'firmware-utils', 'host' }

define_rule { 'firmware-utils', 'target',
    requires = { 'gpgme' }
}

local firmware_rule = {
    config = 'target',
    install = {
        prefix = '/usr'
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
        'hostapd',
        'libass',
        'libuv',
        'soundtouch',
        'wpa_supplicant',
    }
}

if Jagen.flag 'debug' then
    define_rule { 'gdb', 'host' }
    define_rule { 'strace', 'target' }
end

require 'chicken'
