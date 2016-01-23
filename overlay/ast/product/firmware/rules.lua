-- Sigma rules

local function rootfs_package(rule)
    rule.template = { config = 'target',
        { 'build', { 'rootfs', 'build' } },
        install = {
            path = '$jagen_sdk_staging_dir',
            prefix = ''
        }
    }
    package(rule)
end

local function kernel_package(rule)
    rule.template = { config = 'target',
        { 'build', { 'kernel', 'build' } }
    }
    package(rule)
end

local function firmware_package(rule)
    rule.template = { config = 'target',
        { 'install', { 'firmware', 'unpack' } }
    }
    package(rule)
end

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

if jagen.flag('debug') then
    package        { 'gdb', 'host' }
    rootfs_package { 'gdbserver' }
end

-- host

package { 'utils', 'host' }

package { 'karaoke-player', 'host',
    { 'build',
        { 'astindex',     'unpack'          },
        { 'chicken',      'install', 'host' },
        { 'chicken-eggs', 'install', 'host' },
        { 'ffmpeg',       'install', 'host' },
        { 'libuv',        'install', 'host' }
    }
}

package { 'astindex',
    { 'unpack', { 'karaoke-player', 'unpack' } }
}

package { 'chicken', 'host' }

package { 'chicken-eggs', 'host',
    { 'install',
        { 'chicken', 'install', 'host' }
    }
}

package { 'ffmpeg', 'host' }

package { 'libuv', 'host' }

-- kernel

package { 'kernel',
    source = { branch = 'sigma-2.6' },
    { 'build',
        { 'ezboot', 'build', 'target' },
        { 'linux',  'unpack' },
        { 'rootfs', 'build'  },
    },
    { 'install' },
    { 'image',  { 'rootfs', 'install' } }
}

kernel_package { 'loop-aes' }

kernel_package { 'ralink' }

-- rootfs

package { 'rootfs',
    { 'build',
        { 'ast-files',  'unpack'            },
        { 'make',       'install', 'tools'  },
        { 'xsdk',       'unpack'            },
    },
    { 'install',
        { 'busybox',    'install', 'target' },
        { 'gnupg',      'install', 'target' },
        { 'kernel',     'install'           },
        { 'loop-aes',   'install', 'target' },
        { 'mrua',       'modules',          },
        { 'ntpclient',  'install', 'target' },
        { 'ralink',     'install', 'target' },
        { 'util-linux', 'install', 'target' },
        { 'utils',      'install', 'target' },
    }
}

package { 'mrua',
    { 'build',  { 'kernel',   'build'  } },
    { 'modules' },
    { 'install' },
}

rootfs_package { 'ezboot',
    { 'build',
        { 'make', 'install', 'tools' }
    }
}

rootfs_package { 'busybox',
    { 'patch', { 'ast-files', 'unpack' } }
}

rootfs_package { 'gnupg' }

rootfs_package { 'ntpclient' }

rootfs_package { 'util-linux' }

rootfs_package { 'utils',
    { 'build',
        requires = { 'gpgme' },
        { 'dbus', 'install', 'target' }
    }
}

-- firmware

package { 'firmware',
    { 'build',
        { 'mrua', 'build' }
    },
    { 'install',
        { 'kernel',         'image'             },
        { 'mrua',           'install'           },
        { 'ucode',          'install'           },
        { 'ezboot',         'install', 'target' },
        { 'karaoke-player', 'install', 'target' },
        { 'rsync',          'install', 'target' },
        { 'wpa_supplicant', 'install', 'target' },
    },
    { 'strip' }
}

firmware_package { 'karaoke-player',
    { 'build',
        requires = {
            'cairo',
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

firmware_package { 'chicken',
    { 'build', { 'chicken',  'install', 'host' } }
}

firmware_package { 'chicken-eggs',
    { 'install',
        requires = {
            'chicken',
            'dbus',
            'sqlite',
        },
        { 'chicken-eggs', 'install', 'host' },
    }
}

firmware_package { 'rsync' }

firmware_package { 'wpa_supplicant' }
