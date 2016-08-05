-- Sigma rules

-- base

define_rule { 'ast-files' }

define_rule { 'cmake-modules' }

define_rule { 'linux' }

define_rule { 'xsdk' }

define_rule { 'ucode', 'target',
    { 'install' }
}

-- host

define_rule { 'utils', 'host' }

define_rule { 'karaoke-player', 'host',
    requires = {
        'chicken-eggs',
        'ffmpeg',
        'libuv',
    },
    { 'configure',
        { 'astindex', 'unpack' },
    }
}

define_rule { 'astindex',
    { 'unpack', { 'karaoke-player', 'unpack' } }
}

-- kernel

define_rule { 'kernel', 'target',
    { 'configure',
        { 'linux', 'unpack' },
    },
    { 'compile',
        { 'rootfs', 'compile', 'target' }
    },
    { 'image',
        requires = {
            'loop-aes',
            'ralink',
            'rootfs',
        }
    }
}

-- rootfs

local rootfs_template = {
    config = 'target',
    { 'install',
        { 'rootfs', 'compile', 'target' }
    }
}

define_rule { 'rootfs', 'target',
    pass_template = rootfs_template,
    { 'configure',
        { 'ast-files', 'unpack' },
        { 'xsdk',      'unpack' },
    },
    { 'install',
        requires = {
            'busybox',
            'gnupg',
            'loop-aes',
            'mrua',
            'ntpclient',
            'ralink',
            'util-linux',
            'utils',
        }
    }
}

define_rule { 'busybox', 'target',
    install = {
        root = '$jagen_sdk_initfs_dir',
        prefix = ''
    },
    { 'patch', { 'ast-files', 'unpack' } }
}

define_rule { 'utils', 'target',
    requires = { 'gpgme' },
    { 'configure',
        { 'dbus', 'install', 'target' }
    }
}

define_rule { 'ezboot', 'target',
    requires = { 'rootfs' }
}

-- firmware

local firmware_rule_template = {
    config = 'target',
    install = {
        prefix = '/firmware'
    },
    { 'install', { 'firmware', 'unpack' } }
}

local function define_firmware_rule(r)
    r.template = firmware_rule_template
    define_rule(r)
end

define_rule { 'firmware', 'target',
    pass_template = firmware_rule_template,
    requires = {
        'ezboot',
        'karaoke-player',
        'kernel',
        'mrua',
        'rsync',
        'ucode',
        'wpa_supplicant',
    },
    install = {
        prefix = '/firmware'
    },
    { 'compile' },
    { 'install' }
}

define_firmware_rule { 'karaoke-player',
    requires = {
        'chicken-eggs',
        'connman',
        'dbus',
        'ffmpeg',
        'freetype',
        'libass',
        'libpng',
        'libuv',
        'mrua',
        'soundtouch',
        'sqlite',
    },
    { 'configure',
        { 'astindex', 'unpack' },
        { 'chicken-eggs', 'install', 'host' }
    }
}

-- additional packages should come last to apply all templates defined here

require 'chicken'
