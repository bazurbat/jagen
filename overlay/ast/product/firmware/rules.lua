-- Sigma rules

-- base

package { 'ast-files' }

package { 'cmake-modules' }

package { 'linux' }

package { 'xsdk' }

package { 'ucode',
    { 'install' }
}

-- host

package { 'utils', 'host' }

package { 'karaoke-player', 'host',
    { 'configure',
        requires = {
            'chicken-eggs',
            'ffmpeg',
            'libuv',
        },
        { 'astindex', 'unpack' },
    }
}

package { 'astindex',
    { 'unpack', { 'karaoke-player', 'unpack' } }
}

-- kernel

local kernel_package_template = {
    config = 'target',
    { 'configure', { 'kernel', 'compile', 'target' } }
}

local function kernel_package(rule)
    package(rule, kernel_package_template)
end

package { 'kernel', 'target',
    { 'configure',
        { 'linux',  'unpack'            },
        { 'rootfs', 'compile'           },
        { 'ezboot', 'compile', 'target' },
    },
    { 'install' },
    { 'image',  { 'rootfs', 'install' } }
}

kernel_package { 'loop-aes' }

kernel_package { 'ralink' }

-- rootfs

local rootfs_package_template = {
    config  = 'target',
    install = {
        root = '$jagen_sdk_staging_dir',
        prefix = ''
    },
    { 'configure', { 'rootfs', 'compile' } }
}

local function rootfs_package(rule)
    package(rule, rootfs_package_template)
end

package { 'rootfs',
    template = rootfs_package_template,
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

package { 'mrua', 'target',
    { 'compile',  { 'kernel', 'compile', 'target' } },
    { 'modules' },
    { 'install' },
}

rootfs_package { 'ezboot' }

rootfs_package { 'busybox',
    install = {
        root = '$jagen_sdk_initfs_dir',
        prefix = ''
    },
    { 'patch', { 'ast-files', 'unpack' } }
}

rootfs_package { 'utils',
    { 'configure',
        requires = { 'gpgme' },
        { 'dbus', 'install', 'target' }
    }
}

-- firmware

local firmware_package_template = {
    config = 'target',
    install = {
        prefix = '/firmware'
    },
    { 'install', { 'firmware', 'unpack' } }
}

local function firmware_package(rule)
    package(rule, firmware_package_template)
end

package { 'firmware',
    template = firmware_package_template,
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

firmware_package { 'karaoke-player',
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
