-- HiSilicon Linux SDK

local rootfs_rule = {
    config = 'target',
    { 'install',
        { 'rootfs', 'compile', 'target' }
    }
}

local firmware_rule = {
    config = 'target',
    install = {
        prefix = '/usr'
    },
    { 'install',
        { 'firmware', 'compile', 'target' }
    }
}

if not Jagen.flag 'devenv' then

    define_rule { 'hi-kernel', 'target',
        template = rootfs_rule,
        { 'compile',
            { 'initramfs', 'configure', 'target' }
        },
        { 'image',
            requires = { 'initramfs' }
        }
    }

    define_rule { 'initramfs', 'target',
        { 'configure',
            { 'hi-kernel', 'configure', 'target' },
        },
        { 'install',
            requires = {
                'busybox',
                'loop-aes',
                'util-linux',
            }
        }
    }

end

define_rule { 'loop-aes', 'target',
    requires = {
        'hi-kernel'
    }
}

define_rule { 'hi-sdk', 'target',
    { 'compile',
        { 'hi-kernel', 'configure', 'target' }
    }
}

-- explicit definition of firmware utils to avoid building gpgme for host
define_rule { 'firmware-utils', 'host' }
define_rule { 'firmware-utils', 'target',
    requires = { 'gpgme' }
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

define_rule { 'firmware', 'target',
    pass_template = firmware_rule,
    { 'compile',
        { 'rootfs', 'compile', 'target' }
    },
    { 'install',
        requires = {
            'hi-utils',
            'karaoke-player'
        }
    }
}

define_rule { 'rootfs', 'target',
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
            'hi-sdk',
            'hia-astdisplayservice',
            'loop-aes',
            'rtl8188eu',
            'util-linux',
        }
    }
}

if Jagen.flag 'debug' then
    define_rule { 'gdb', 'host' }
    define_rule { 'strace', 'target' }
end

require 'chicken'
