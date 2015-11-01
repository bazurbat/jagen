-- Sigma rules

local function rootfs_package(rule)
    package(rule, {
            { 'build', { 'rootfs', 'build' } },
        })
end

-- base

package { 'ast-files' }

package { 'linux',
    source = { branch = 'ast50' }
}

package { 'xsdk',
    source = '${cpukeys}.tar.gz'
}

package { 'ucode',
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
    package(rule, {
            { 'build', { 'kernel', 'build' } },
        })
end

package { 'kernel',
    source = { branch = 'sigma-2.6' },
    { 'build',
        { 'ezboot', 'build'  },
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
        { 'busybox',    'install'           },
        { 'gnupg',      'install'           },
        { 'kernel',     'install'           },
        { 'loop-aes',   'install'           },
        { 'mrua',       'modules'           },
        { 'ntpclient',  'install'           },
        { 'ralink',     'install'           },
        { 'util-linux', 'install'           },
        { 'utils',      'install', 'target' },
    }
}

package { 'mrua',
    { 'build',   { 'kernel',   'build'  } },
    { 'modules'  },
    { 'install', { 'firmware', 'unpack' } }
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
        { 'dbus',  'install', 'target' },
        { 'gpgme', 'install' },
    }
}

rootfs_package { 'libgpg-error' }

rootfs_package { 'libassuan',
    { 'build', { 'libgpg-error', 'install' } }
}

rootfs_package { 'gpgme',
    { 'build', { 'libassuan', 'install' } }
}

-- firmare

local function firmware_package(rule)
    package(rule, {
            config = 'target',
            { 'install', { 'firmware', 'unpack' } },
        })
end

firmware_package { 'firmware', 'target',
    { 'material',
        { 'mrua', 'build' }
    },
    { 'install',
        { 'ezboot', 'install' },
        { 'kernel', 'image'   },
        { 'mrua',   'install' },
        needs = {
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
        { 'astindex',     'unpack'            },
        { 'mrua',         'build'             },
        { 'chicken-eggs', 'install', 'host'   },
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
        { 'chicken-eggs', 'install', 'host'   },
        needs = {
            'chicken',
            'dbus',
            'sqlite'
        }
    }
}
