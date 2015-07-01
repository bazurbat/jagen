local function rootfs_package(rule)
    local pkg = {
        name = rule.name,
        { 'build', { 'rootfs', 'build' } },
        { 'install' }
    }
    package(pkg, rule)
end

local function kernel_package(rule)
    local pkg = {
        name = rule.name,
        { 'build', { 'kernel', 'build' } },
        { 'install' }
    }
    package(pkg, rule)
end

local function firmware_package(rule)
    local pkg = {
        name = rule.name,
        { 'build' },
        { 'install', { 'firmware', 'unpack' } }
    }
    package(pkg, rule)
end

-- base

package {
    name = 'ast-files'
}

package {
    name = 'linux',
    source = {
        branch = 'ast50'
    }
}

package {
    name = 'xsdk',
    source = '${cpukeys}.tar.gz'
}

package {
    name = 'ucode',
    { 'unpack',  { 'mrua',     'build'  } },
    { 'install', { 'firmware', 'unpack' } }
}

-- tools

package {
    name = 'make',
    { 'build',   'host' },
    { 'install', 'host' }
}

-- host

package {
    name = 'libtool',
    { 'build'   },
    { 'install' }
}

-- utils

package {
    name = 'utils',
    { 'build',   'host' },
    { 'install', 'host' }
}

package {
    name = 'utils',
    { 'build',   'target',
        { 'dbus',  'install' },
        { 'gpgme', 'install' },
    },
    { 'install', 'target' }
}

-- boot

rootfs_package {
    name = 'ezboot',
    source = {
        branch = "sdk4"
    }
}

-- debugging

if jagen.flag('debug') then
    package {
        name = 'gdb',
        { 'build',   'host' },
        { 'install', 'host' }
    }

    package {
        name   = 'valgrind',
        config = 'rootfs',
        { 'build'   },
        { 'install' }
    }

    rootfs_package {
        name = 'gdbserver'
    }

    rootfs_package {
        name = 'strace'
    }
end

-- rootfs

package {
    name = 'rootfs',
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

rootfs_package {
    name = 'busybox',
    { 'patch', { 'ast-files', 'unpack' } }
}

rootfs_package {
    name = 'ntpclient'
}

rootfs_package {
    name = 'util-linux'
}

-- gpgme

rootfs_package {
    name = 'libgpg-error'
}

rootfs_package {
    name = 'libassuan',
    { 'build', { 'libgpg-error', 'install' } }
}

rootfs_package {
    name = 'gpgme',
    { 'build', { 'libassuan', 'install' } }
}

rootfs_package {
    name = 'gnupg'
}

-- kernel

package {
    name = 'kernel',
    source = {
        branch = 'sigma-2.6'
    },
    { 'build',
        { 'ezboot', 'build'  },
        { 'linux',  'unpack' },
        { 'rootfs', 'build'  },
    },
    { 'install' },
    { 'image', { 'rootfs', 'install' } }
}

kernel_package {
    name = 'ralink'
}

kernel_package {
    name = 'loop-aes'
}

package {
    name = 'mrua',
    source = {
        branch = 'sigma-2.6'
    },
    { 'build',   { 'kernel',   'build'  } },
    { 'modules'  },
    { 'install', { 'firmware', 'unpack' } }
}

package {
    name = 'chicken',
    { 'build',   'host' },
    { 'install', 'host' }
}

package {
    name = 'chicken',
    { 'build',   'target',
        { 'chicken',  'install', 'host' }
    },
    { 'install', 'target',
        { 'firmware', 'unpack'          }
    }
}

package {
    name = 'chicken-eggs',
    { 'install', 'host',
        { 'chicken', 'install', 'host'        }
    }
}

package {
    name = 'chicken-eggs',
    { 'install', 'target',
        { 'toolchain'                         },
        { 'chicken',      'install', 'target' },
        { 'chicken-eggs', 'install', 'host'   },
        { 'dbus',         'install'           },
        { 'sqlite',       'install', 'target' },
    }
}

package {
    name = 'libuv',
    { 'build',   'host'   },
    { 'install', 'host'   }
}

package {
    name = 'libuv',
    { 'build',   'target' },
    { 'install', 'target',
        { 'firmware', 'unpack' }
    }
}

firmware_package {
    name = 'dbus',
    { 'build', { 'expat', 'install', 'target' } }
}

firmware_package {
    name   = 'expat',
    config = 'target'
}

firmware_package {
    name   = 'freetype',
    config = 'target'
}

firmware_package {
    name   = 'rsync',
    config = 'target'
}

firmware_package {
    name   = 'sqlite',
    config = 'target'
}

firmware_package {
    name   = 'libnl',
    config = 'target'
}

firmware_package {
    name = 'wpa_supplicant',
    { 'build',
        { 'dbus',  'install'           },
        { 'libnl', 'install', 'target' },
    }
}

firmware_package {
    name = 'zlib'
}

firmware_package {
    name   = 'libpng',
    config = 'target',
    { 'build', { 'zlib', 'install' } }
}

firmware_package {
    name   = 'xtables',
    config = 'target'
}

firmware_package {
    name   = 'xtables-addons',
    config = 'target',
    { 'build', { 'xtables', 'install', 'target' } }
}

package {
    name = 'ffmpeg',
    { 'build',   'host',
        { 'ast-files', 'unpack' }
    },
    { 'install', 'host' }
}

package {
    name = 'ffmpeg',
    { 'build',   'target',
        { 'ast-files', 'unpack' }
    },
    { 'install', 'target',
        { 'firmware',  'unpack' }
    }
}

firmware_package {
    name = 'soundtouch'
}

package {
    name = 'astindex',
    { 'unpack', { 'karaoke-player', 'unpack' } }
}

package {
    name = 'karaoke-player',
    { 'build', 'host',
        { 'astindex',     'unpack'            },
        { 'chicken-eggs', 'install', 'host'   },
        { 'ffmpeg',       'install', 'host'   },
        { 'libuv',        'install', 'host'   },
    },
    { 'install', 'host'                       }
}

package {
    name = 'karaoke-player',
    { 'build',   'target',
        { 'astindex',     'unpack'            },
        { 'chicken',      'install', 'target' },
        { 'chicken-eggs', 'install', 'host'   },
        { 'connman',      'install'           },
        { 'dbus',         'install'           },
        { 'ffmpeg',       'install', 'target' },
        { 'freetype',     'install', 'target' },
        { 'libass',       'install', 'target' },
        { 'libpng',       'install', 'target' },
        { 'libuv',        'install', 'target' },
        { 'mrua',         'build'             },
        { 'soundtouch',   'install'           },
    },
    { 'install', 'target',
        { 'chicken-eggs', 'install', 'target' }
    }
}

package {
    name = 'firmware',
    { 'material',
        { 'mrua',           'build'             }
    },
    { 'install',
        { 'connman',        'install',          },
        { 'dbus',           'install',          },
        { 'expat',          'install', 'target' },
        { 'ezboot',         'install',          },
        { 'freetype',       'install', 'target' },
        { 'glib',           'install',          },
        { 'karaoke-player', 'install', 'target' },
        { 'kernel',         'image'             },
        { 'libffi',         'install', 'target' },
        { 'libpng',         'install', 'target' },
        { 'mrua',           'install'           },
        { 'rsync',          'install', 'target' },
        { 'sqlite',         'install', 'target' },
        { 'wpa_supplicant', 'install',          },
        { 'zlib',           'install',          },
    },
    { 'strip' }
}

firmware_package {
    name   = 'libffi',
    config = 'target'
}

firmware_package {
    name = 'glib',
    { 'build',
        { 'zlib',     'install' },
        { 'libffi',   'install', 'target' },
    }
}

firmware_package {
    name = 'connman',
    { 'build',
        { 'dbus',           'install'           },
        { 'glib',           'install'           },
        { 'xtables-addons', 'install', 'target' },
    }
}

firmware_package {
    name   = 'fribidi',
    config = 'target',
    { 'build',
        { 'glib', 'install' }
    }
}

firmware_package {
    name   = 'libass',
    config = 'target',
    { 'build',
        { 'freetype', 'install', 'target' },
        { 'fribidi',  'install', 'target' },
    }
}
