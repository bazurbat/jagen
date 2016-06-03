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
    requires = {
        'chicken-eggs',
        'ffmpeg',
        'libuv',
    },
    { 'configure',
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
    template = kernel_rule_template,
    skip_template = true,
    requires = { 'rootfs' },
    { 'configure',
        { 'linux', 'unpack' },
    },
    { 'install' },
}

kernel_rule { 'loop-aes' }

kernel_rule { 'ralink' }

-- rootfs

local rootfs_rule_template = {
    config  = 'target'
}

local function rootfs_rule(r)
    r.template = rootfs_rule_template
    Pkg:add(r)
end

Pkg:add { 'rootfs', 'target',
    template = rootfs_rule_template,
    skip_template = true,
    requires = {
        'busybox',
        'gnupg',
        'loop-aes',
        'mrua',
        'ntpclient',
        'ralink',
        'util-linux',
        'utils',
    },
    { 'configure',
        { 'ast-files', 'unpack'            },
        { 'xsdk',      'unpack'            },
        { 'toolchain', 'install', 'target' },
    },
    { 'compile' },
    { 'install' }
}

Pkg:add { 'mrua', 'target',
    { 'compile',
        { 'kernel', 'compile', 'target' }
    },
    { 'install' },
}

rootfs_rule { 'busybox',
    install = {
        root = '$jagen_sdk_initfs_dir',
        prefix = ''
    },
    { 'patch', { 'ast-files', 'unpack' } }
}

rootfs_rule { 'utils',
    requires = { 'gpgme' },
    { 'configure',
        { 'dbus', 'install', 'target' }
    }
}

Pkg:add { 'ezboot', 'target',
    requires = { 'rootfs' }
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
        prefix = '$jagen_firmware_install_prefix'
    },
    { 'compile' },
    { 'install' }
}

Pkg:add { 'chicken-eggs', 'target',
    requires = { 'dbus', 'sqlite' }
}

firmware_rule { 'karaoke-player',
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
