-- Sigma rules

-- base

package { 'ast-files' }

package { 'linux',
    source = { branch = 'ast50' }
}

package { 'xsdk' }

package { 'ucode',
    { 'install' }
}

-- tools

package { 'make', 'tools' }

-- FIXME: pkg scripts need review
-- if jagen.flag('debug') then
--     package        { 'gdb', 'host' }
--     rootfs_package { 'gdbserver' }
-- end

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
    { 'configure', { 'kernel', 'build' } }
}

local function kernel_package(rule)
    package(rule, kernel_package_template)
end

package { 'kernel',
    source = { branch = 'sigma-2.6' },
    { 'configure',
        { 'linux',  'unpack' },
        { 'rootfs', 'build'  },
        { 'ezboot', 'build', 'target' },
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
        path = '$jagen_sdk_staging_dir',
        prefix = ''
    },
    { 'configure', { 'rootfs', 'build' } }
}

local function rootfs_package(rule)
    package(rule, rootfs_package_template)
end

package { 'rootfs',
    template = rootfs_package_template,
    { 'configure',
        { 'ast-files', 'unpack'           },
        { 'xsdk',      'unpack'           },
        { 'make',      'install', 'tools' },
    },
    { 'install',
        requires = {
            'busybox',
            'gnupg',
            'ntpclient',
            'util-linux',
            'utils',
        },
        { 'kernel',   'install'           },
        { 'mrua',     'modules',          },
        { 'loop-aes', 'install', 'target' },
        { 'ralink',   'install', 'target' },
    }
}

package { 'mrua',
    { 'configure',  { 'kernel', 'build' } },
    { 'modules' },
    { 'install' },
}

rootfs_package { 'ezboot',
    { 'configure',
        { 'make', 'install', 'tools' }
    }
}

rootfs_package { 'busybox',
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
    { 'install', { 'firmware', 'unpack' } }
}

local function firmware_package(rule)
    package(rule, firmware_package_template)
end

package { 'firmware',
    template = firmware_package_template,
    { 'build',
        { 'mrua', 'build' }
    },
    { 'install',
        requires = {
            'karaoke-player',
            'rsync',
            'wpa_supplicant',
        },
        { 'kernel', 'image'             },
        { 'mrua',   'install'           },
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
        { 'astindex',     'unpack'          },
        { 'mrua',         'build',          },
        { 'chicken-eggs', 'install', 'host' },
    }
}
