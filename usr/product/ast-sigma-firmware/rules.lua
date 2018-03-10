-- Sigma rules

-- base

package { 'ast-files' }

package { 'cmake-modules' }

package { 'linux' }

package { 'xsdk' }

package { 'ucode', 'target',
    { 'install' }
}

package { 'rootfs', 'target' }

package { 'ezboot', 'target' }

-- host

package { 'utils', 'host' }

package { 'karaoke-player', 'host',
    requires = {
        'chicken-eggs',
        'ffmpeg',
        'libuv',
    },
    { 'configure',
        { 'astindex', 'unpack' },
    }
}

package { 'astindex',
    { 'unpack', { 'karaoke-player', 'unpack' } }
}

-- kernel

package { 'kernel', 'target',
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

package { 'loop-aes',
    use = { 'kernel', 'rootfs' },
    env = {
        INSTALL_MOD_PATH = '$rootfs_root'
    },
}

-- rootfs

local rootfs_template = {
    config = 'target',
    { 'install',
        { 'rootfs', 'compile', 'target' }
    }
}

package { 'rootfs', 'target',
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

package { 'busybox', 'target',
    use = 'rootfs',
    install = {
        root = '$rootfs_root',
        prefix = ''
    },
    { 'patch', { 'ast-files', 'unpack' } }
}

package { 'utils', 'target',
    requires = { 'gpgme' },
    { 'configure',
        { 'dbus', 'install', 'target' }
    }
}

package { 'ezboot', 'target',
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

local function firmware_package(r)
    r.template = firmware_rule_template
    return package(r)
end

package { 'firmware', 'target',
    pass_template = firmware_rule_template,
    use = 'mrua',
    requires = {
        'ezboot',
        'karaoke-player',
        'mrua',
        'rsync',
        'ucode',
        'wpa_supplicant',
    },
    install = {
        prefix = '/firmware'
    },
    { 'compile' },
    { 'install',
        { 'kernel', 'image', 'target' }
    }
}

firmware_package { 'karaoke-player',
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
    },
    use = { 'mrua', 'rootfs' }
}

-- additional packages should come last to apply all templates defined here

require 'chicken'
