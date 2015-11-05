-- Sigma rules

local function rootfs_package(rule)
    rule.template = {
        config = 'target',
        { 'build', { 'rootfs', 'build' } },
    }
    package(rule)
end

-- base

package { 'ast-files' }

package { 'linux',
    source = { branch = 'ast50' }
}

package { 'xsdk',
    source = '${cpukeys}.tar.gz'
}

package { 'ucode', 'target',
    { 'unpack',  { 'mrua',     'build'  } },
    { 'install', { 'firmware', 'unpack' } }
}

-- tools

package { 'make', 'tools' }

if jagen.flag('debug') then
    package        { 'gdb', 'host' }
    rootfs_package { 'gdbserver' }
    package        { 'valgrind', 'rootfs' }
    rootfs_package { 'strace',   'rootfs' }
end

-- host

package { 'astindex',
    { 'unpack', { 'karaoke-player', 'unpack' } }
}

package { 'chicken', 'host' }

package { 'chicken-eggs', 'host',
    { 'install',
        needs = { 'chicken' }
    }
}

package { 'ffmpeg', 'host',
    { 'build', { 'ast-files', 'unpack' } }
}

package { 'utils', 'host' }

package { 'karaoke-player', 'host',
    source = { branch = 'master' },
    { 'build',
        { 'astindex', 'unpack' },
        needs = {
            'chicken',
            'chicken-eggs',
            'ffmpeg',
            'libuv'
        }
    }
}

package { 'libtool', 'host' }

-- kernel

local function kernel_package(rule)
    rule.template = {
        config = 'target',
        { 'build', { 'kernel', 'build' } },
    }
    package(rule)
end

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
    { 'build',   { 'kernel',   'build'  } },
    { 'modules'  },
    { 'install', { 'firmware', 'unpack' } },
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

rootfs_package { 'utils', 'target',
    { 'build',
        needs = { 'dbus', 'gpgme' }
    }
}

rootfs_package { 'libgpg-error' }

rootfs_package { 'libassuan',
    { 'build',
        needs = { 'libgpg-error' }
    }
}

rootfs_package { 'gpgme',
    { 'build',
        needs = { 'libassuan' }
    }
}

-- firmare

local function firmware_package(rule)
    rule.template = {
        config = 'target',
        { 'install', { 'firmware', 'unpack' } },
    }
    package(rule)
end

package { 'firmware',
    template = {
        config = 'target',
        { 'install', { 'firmware', 'unpack' } }
    },

    { 'material', { 'mrua',   'build' } },
    { 'install',
        { 'kernel', 'image'   },
        { 'mrua', 'install'   },
        needs = {
            'ezboot',
            'karaoke-player',
            'rsync',
            'wpa_supplicant',
        }
    },
    { 'strip' }
}

firmware_package { 'karaoke-player',
    source = { branch = 'master' },
    { 'build',
        { 'astindex',     'unpack'          },
        { 'chicken-eggs', 'install', 'host' },
        { 'mrua',         'build',          },
        needs = {
            'chicken-eggs',
            'connman',
            'dbus',
            'ffmpeg',
            'freetype',
            'libass',
            'libpng',
            'libuv',
            'soundtouch'
        }
    }
}

firmware_package { 'chicken',
    { 'build', { 'chicken',  'install', 'host' } }
}

firmware_package { 'chicken-eggs',
    { 'install',
        { 'chicken-eggs', 'install', 'host' },
        needs = {
            'chicken',
            'dbus',
            'sqlite'
        }
    }
}
