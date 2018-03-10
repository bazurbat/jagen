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

package { 'hi-sdk', 'target',
    { 'configure',
        { 'kernel', 'unpack' }
    }
}

package { 'karaoke-player', 'target',
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

package { 'hi-utils', 'target',
    requires = { 'karaoke-player' }
}

package { 'firmware', 'target',
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

package { 'rootfs', 'target',
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
            'kernel',
            'hi-sdk',
            'hia-astdisplayservice',
            'loop-aes',
            'ntfs3g',
            'rtl8188eu',
            'util-linux',
        }
    }
}

package { 'loop-aes',
    use = { 'kernel' },
}

-- explicit definition of firmware utils to avoid building gpgme for host
package { 'firmware-utils', 'host' }
package { 'firmware-utils', 'target',
    requires = { 'gpgme' }
}

package { 'image', 'target',
    { 'compile',
        requires = {
            'rootfs'
        }
    }
}

if Jagen.flag 'debug' then
    package { 'gdb', 'host' }
    package { 'strace', 'target' }
    package { 'gdbserver', 'target' }
end

require 'chicken'
