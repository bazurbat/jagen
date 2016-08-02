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

define_rule { 'kernel', 'target',
    { 'compile',
        { 'hi-kernel', 'compile', 'target' }
    }
}

if not Jagen.flag 'devenv' then

    define_rule { 'hi-kernel', 'target',
        template = rootfs_template_rule,
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

-- explicit definition of firmware utils to avoid building gpgme for host
define_rule { 'firmware-utils', 'host' }
define_rule { 'firmware-utils', 'target',
    requires = { 'gpgme' }
}

define_rule { 'karaoke-player', 'target',
    template = firmware_template_rule,
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
    pass_template = firmware_template_rule,
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
