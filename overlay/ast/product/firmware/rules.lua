-- Sigma rules

-- base

rule { 'ast-files' }

rule { 'cmake-modules' }

rule { 'linux' }

rule { 'xsdk' }

rule { 'ucode',
    { 'install' }
}

-- host

rule { 'utils', 'host' }

rule { 'karaoke-player', 'host',
    { 'configure',
        requires = {
            'chicken-eggs',
            'ffmpeg',
            'libuv',
        },
        { 'astindex', 'unpack' },
    }
}

rule { 'astindex',
    { 'unpack', { 'karaoke-player', 'unpack' } }
}

-- kernel

local kernel_rule_template = {
    config = 'target',
    { 'configure', { 'kernel', 'compile', 'target' } }
}

local function kernel_rule(r)
    r.template = kernel_rule_template
    rule(r)
end

rule { 'kernel', 'target',
    { 'configure',
        { 'linux',  'unpack'            },
        { 'rootfs', 'compile'           },
        { 'ezboot', 'compile', 'target' },
    },
    { 'install' },
    { 'image',  { 'rootfs', 'install' } }
}

kernel_rule { 'loop-aes' }

kernel_rule { 'ralink' }

-- rootfs

local rootfs_rule_template = {
    config  = 'target',
    { 'configure', { 'rootfs', 'compile' } }
}

local function rootfs_rule(r)
    r.template = rootfs_rule_template
    rule(r)
end

rule { 'rootfs', 'target',
    template = rootfs_rule_template,
    skip_template = true,
    { 'configure',
        { 'ast-files', 'unpack'            },
        { 'xsdk',      'unpack'            },
        { 'toolchain', 'install', 'target' },
    },
    { 'compile' },
    { 'install',
        requires = {
            'busybox',
            'gnupg',
            'ntpclient',
            'util-linux',
            'utils',
        },
        { 'kernel',   'install', 'target' },
        { 'mrua',     'modules', 'target' },
        { 'loop-aes', 'install', 'target' },
        { 'ralink',   'install', 'target' },
    },
    { 'strip' }
}

rule { 'mrua', 'target',
    { 'compile',  { 'kernel', 'compile', 'target' } },
    { 'modules' },
    { 'install' },
}

rootfs_rule { 'ezboot' }

rootfs_rule { 'busybox',
    install = {
        root = '$jagen_sdk_initfs_dir',
        prefix = ''
    },
    { 'patch', { 'ast-files', 'unpack' } }
}

rootfs_rule { 'utils',
    { 'configure',
        requires = { 'gpgme' },
        { 'dbus', 'install', 'target' }
    }
}

-- firmware

local firmware_rule_template = {
    config = 'target',
    install = {
        prefix = '/firmware'
    },
    { 'install', { 'firmware', 'unpack' } }
}

local function firmware_rule(r)
    r.template = firmware_rule_template
    rule(r)
end

rule { 'firmware',
    template = firmware_rule_template,
    skip_template = true,
    { 'compile',
        { 'mrua', 'compile', 'target' }
    },
    { 'install',
        requires = {
            'karaoke-player',
            'rsync',
            'wpa_supplicant',
        },
        { 'kernel', 'image',   'target' },
        { 'mrua',   'install', 'target' },
        { 'ucode',  'install'           },
        { 'ezboot', 'install', 'target' },
    },
    { 'strip' }
}

firmware_rule { 'karaoke-player',
    { 'configure',
        requires = {
            'chicken-eggs',
            'connman',
            'dbus',
            'ffmpeg',
            'freetype',
            'libass',
            'libpng',
            'libuv',
            'soundtouch',
        },
        { 'astindex',     'unpack'            },
        { 'mrua',         'compile', 'target' },
        { 'chicken-eggs', 'install', 'host'   },
    }
}
