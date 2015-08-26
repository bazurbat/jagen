-- Sigma rules

local function rootfs_package(rule)
    local pkg = {
        { 'build', { 'rootfs', 'build' } },
        { 'install' }
    }
    package(pkg, rule)
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

package { 'make', 'host' }

if jagen.flag('debug') then
    package        { 'gdb', 'host' }
    rootfs_package { 'gdbserver' }
    package        { 'valgrind', 'rootfs' }
    rootfs_package { 'strace' }
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
    local pkg = {
        { 'build', { 'kernel', 'build' } },
        { 'install' }
    }
    package(pkg, rule)
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
        { 'make',       'install', 'host'   },
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
    source = { branch = "sdk4" }
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
    local pkg = {
        config = 'target',
        { 'build' },
        { 'install', { 'firmware', 'unpack' } },
        inject = {
            { 'install', { 'firmware', 'unpack' } }
        }
    }
    package(pkg, rule)
end

package { 'firmware', 'target',
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
    },
    { 'install',
        needs = { 'chicken-eggs' }
    }
}

firmware_package { 'chicken',
    { 'build', { 'chicken',  'install', 'host' } }
}

package { 'chicken-eggs', 'target',
    { 'install',
        { 'chicken-eggs', 'install', 'host'   },
        needs = {
            'chicken',
            'dbus',
            'sqlite'
        }
    }
}

firmware_package { 'ffmpeg',
    { 'build', { 'ast-files', 'unpack' } }
}

firmware_package { 'rsync' }

firmware_package { 'sqlite' }

firmware_package { 'wpa_supplicant' }
