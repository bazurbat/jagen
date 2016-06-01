-- Sigma rules

require 'chicken'

-- base

Pkg:add { 'ast-files' }

Pkg:add { 'cmake-modules' }

Pkg:add { 'linux' }

Pkg:add { 'xsdk' }

Pkg:add { 'ucode', 'target',
    { 'install' }
}

-- host

Pkg:add { 'utils', 'host' }

Pkg:add { 'karaoke-player', 'host',
    { 'configure',
        requires = {
            'chicken-eggs',
            'ffmpeg',
            'libuv',
        },
        { 'astindex', 'unpack' },
    }
}

Pkg:add { 'astindex',
    { 'unpack', { 'karaoke-player', 'unpack' } }
}

-- kernel

local kernel_rule_template = {
    config = 'target',
    { 'configure', { 'kernel', 'compile', 'target' } }
}

local function kernel_rule(r)
    r.template = kernel_rule_template
    Pkg:add(r)
end

Pkg:add { 'kernel', 'target',
    { 'configure',
        { 'linux',  'unpack'            },
        { 'rootfs', 'compile', 'target' },
        { 'ezboot', 'compile', 'target' },
    },
    { 'install' },
    { 'image',
        { 'rootfs', 'install', 'target' }
    }
}

kernel_rule { 'loop-aes' }

kernel_rule { 'ralink' }

-- rootfs

local rootfs_rule_template = {
    config  = 'target',
    { 'configure', { 'rootfs', 'compile', 'target' } }
}

local function rootfs_rule(r)
    r.template = rootfs_rule_template
    Pkg:add(r)
end

Pkg:add { 'rootfs', 'target',
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
            'kernel',
            'loop-aes',
            'mrua',
            'ntpclient',
            'ralink',
            'util-linux',
            'utils',
        }
    },
    { 'strip' }
}

Pkg:add { 'mrua', 'target',
    { 'compile',
        { 'kernel', 'compile', 'target' }
    },
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
        prefix = '$jagen_firmware_install_prefix'
    },
    { 'install', { 'firmware', 'unpack' } }
}

local function firmware_rule(r)
    r.template = firmware_rule_template
    Pkg:add(r)
end

Pkg:add { 'firmware', 'target',
    template = firmware_rule_template,
    skip_template = true,
    install = {
        prefix = '$jagen_firmware_install_prefix'
    },
    { 'compile',
        requires = { 'mrua' }
    },
    { 'install',
        requires = {
            'ezboot',
            'karaoke-player',
            'rsync',
            'ucode',
            'wpa_supplicant',
        },
        { 'kernel', 'image',   'target' },
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
            'sqlite',
        },
        { 'astindex',     'unpack'            },
        { 'mrua',         'compile', 'target' },
        { 'chicken-eggs', 'install', 'host'   },
    }
}
