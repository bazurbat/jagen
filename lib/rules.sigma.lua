-- Sigma rules

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

package { 'libuv', 'host' }

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

local function rootfs_package(rule)
    local pkg = {
        { 'build', { 'rootfs', 'build' } },
        { 'install' }
    }
    package(pkg, rule)
end

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
    source = { branch = 'sigma-2.6' },
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
        { 'build' },
        { 'install', { 'firmware', 'unpack' } }
    }
    package(pkg, rule)
end

package { 'firmware',
    { 'material',
        { 'mrua',           'build'             }
    },
    { 'install',
        { 'chicken',        'install', 'target' },
        { 'chicken-eggs',   'install', 'target' },
        { 'connman',        'install', 'target' },
        { 'dbus',           'install', 'target' },
        { 'expat',          'install', 'target' },
        { 'ezboot',         'install',          },
        { 'ffmpeg',         'install', 'target' },
        { 'freetype',       'install', 'target' },
        { 'fribidi',        'install', 'target' },
        { 'glib',           'install', 'target' },
        { 'karaoke-player', 'install', 'target' },
        { 'kernel',         'image'             },
        { 'libass',         'install', 'target' },
        { 'libffi',         'install', 'target' },
        { 'libnl',          'install', 'target' },
        { 'libpng',         'install', 'target' },
        { 'libuv',          'install', 'target' },
        { 'mrua',           'install'           },
        { 'rsync',          'install', 'target' },
        { 'soundtouch',     'install', 'target' },
        { 'sqlite',         'install', 'target' },
        { 'wpa_supplicant', 'install', 'target' },
        { 'xtables',        'install', 'target' },
        { 'xtables-addons', 'install', 'target' },
        { 'zlib',           'install', 'target' },
    },
    { 'strip' }
}

firmware_package { 'chicken', 'target',
    { 'build', { 'chicken',  'install', 'host' } }
}

package { 'chicken-eggs', 'target',
    { 'install',
        { 'toolchain'                         },
        { 'chicken-eggs', 'install', 'host'   },
        needs = {
            'chicken',
            'dbus',
            'sqlite'
        }
    }
}

firmware_package { 'connman', 'target',
    { 'build',
        needs = { 'dbus', 'glib', 'xtables-addons' }
    }
}

firmware_package { 'dbus', 'target',
    { 'build',
        needs = { 'expat' }
    }
}

firmware_package { 'expat', 'target' }

firmware_package { 'ffmpeg', 'target',
    { 'build', { 'ast-files', 'unpack' } }
}

firmware_package { 'freetype', 'target' }

firmware_package { 'fribidi', 'target',
    { 'build',
        needs = { 'glib' }
    }
}

firmware_package { 'glib', 'target',
    { 'build',
        needs = { 'libffi', 'zlib' }
    }
}

firmware_package { 'karaoke-player', 'target',
    { 'build',
        { 'astindex',     'unpack'            },
        { 'mrua',         'build'             },
        { 'chicken-eggs', 'install', 'host'   },
        needs = {
            'chicken',
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

firmware_package { 'libass', 'target',
    { 'build',
        needs = { 'freetype', 'fribidi' }
    }
}

firmware_package { 'libffi', 'target' }

firmware_package { 'libnl', 'target' }

firmware_package { 'libpng', 'target',
    { 'build',
        needs = { 'zlib' }
    }
}

firmware_package { 'libuv', 'target' }

firmware_package { 'rsync', 'target' }

firmware_package { 'soundtouch', 'target' }

firmware_package { 'sqlite', 'target' }

firmware_package { 'wpa_supplicant', 'target',
    { 'build',
        needs = { 'dbus', 'libnl' }
    }
}

firmware_package { 'xtables', 'target' }

firmware_package { 'xtables-addons', 'target',
    { 'build',
        needs = { 'xtables' }
    }
}

firmware_package { 'zlib', 'target' }
