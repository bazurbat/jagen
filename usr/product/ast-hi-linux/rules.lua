-- HiSilicon Linux SDK

local rootfs_template_rule = {
    config = 'target',
    { 'install',
        { 'rootfs', 'compile', 'target' }
    }
}

local firmware_template_rule = {
    config = 'target',
    install = {
        prefix = '/usr'
    },
    { 'install',
        { 'firmware', 'compile', 'target' }
    }
}

define_package_alias('kernel', 'hi-kernel')

define_rule { 'karaoke-player', 'target',
    template = firmware_template_rule,
    requires = {
        'chicken-eggs',
        'connman',
        'dbus',
        'ffmpeg',
        'hi-sdk',
        'libass',
        'libuv',
        'soundtouch',
    }
}

define_rule { 'firmware', 'target',
    pass_template = firmware_template_rule,
    { 'compile',
        { 'rootfs', 'compile', 'target' }
    },
    { 'install',
        requires = {
            'hi-utils',
            'hostapd',
            'karaoke-player',
            'wpa_supplicant',
        }
    }
}

define_rule { 'rootfs', 'target',
    pass_template = rootfs_template_rule,
    { 'install',
        requires = {
            'ast-files',
            'busybox',
            'dropbear',
            'e2fsprogs',
            'firmware',
            'firmware-utils',
            'gnupg',
            'hdparm',
            'hi-drivers',
            'hi-kernel',
            'hi-sdk',
            'hia-astdisplayservice',
            'loop-aes',
            'ntfs3g',
            'rtl8188eu',
            'util-linux',
        }
    }
}

-- explicit definition of firmware utils to avoid building gpgme for host
define_rule { 'firmware-utils', 'host' }
define_rule { 'firmware-utils', 'target',
    requires = { 'gpgme' }
}

define_rule { 'image', 'target',
    { 'compile',
        requires = {
            'rootfs'
        }
    }
}

if Jagen.flag 'debug' then
    define_rule { 'gdb', 'host' }
    define_rule { 'strace', 'target' }
    define_rule { 'gdbserver', 'target' }
end

require 'chicken'
