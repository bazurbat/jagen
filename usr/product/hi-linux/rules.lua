-- HiSilicon Linux SDK

local firmware_rule = {
    config = 'target',
    install = {
        prefix = '/usr'
    }
}

define_rule { 'hi-sdk', 'target',
    { 'compile',
        -- msp modules expect to find compiled kernel in source tree
        { 'hi-kernel', 'compile', 'target' }
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
    }
}

define_rule { 'rootfs', 'target',
    requires = {
        'ast-files',
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
    { 'deploy' }
}

-- explicit definition of firmware utils to avoid building gpgme for host

define_rule { 'firmware-utils', 'host' }

define_rule { 'firmware-utils', 'target',
    requires = { 'gpgme' }
}

if Jagen.flag 'debug' then
    define_rule { 'gdb', 'host' }
    define_rule { 'strace', 'target' }
end

require 'chicken'
