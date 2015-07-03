local function rootfs_package(rule)
    local pkg = {
        { 'build', { 'rootfs', 'build' } },
        { 'install' }
    }
    package(pkg, rule)
end

local function kernel_package(rule)
    local pkg = {
        { 'build', { 'kernel', 'build' } },
        { 'install' }
    }
    package(pkg, rule)
end

local function firmware_package(rule)
    local pkg = {
        { 'build' },
        { 'install', { 'firmware', 'unpack' } }
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

-- host

package { 'libtool', 'host' }

-- utils

package { 'utils', 'host' }

package { 'utils', 'target',
    { 'build',
        { 'dbus',  'install' },
        { 'gpgme', 'install' },
    }
}

-- boot

rootfs_package { 'ezboot',
    source = { branch = "sdk4" }
}

-- debugging

if jagen.flag('debug') then
    package { 'gdb', 'host' }

    package { 'valgrind', 'rootfs' }

    rootfs_package { 'gdbserver' }

    rootfs_package { 'strace' }
end

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

rootfs_package { 'busybox',
    { 'patch', { 'ast-files', 'unpack' } }
}

rootfs_package { 'ntpclient' }

rootfs_package { 'util-linux' }

-- gpgme

rootfs_package { 'libgpg-error' }

rootfs_package { 'libassuan',
    { 'build', { 'libgpg-error', 'install' } }
}

rootfs_package { 'gpgme',
    { 'build', { 'libassuan', 'install' } }
}

rootfs_package { 'gnupg' }

-- kernel

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

kernel_package { 'ralink' }

kernel_package { 'loop-aes' }

package { 'mrua',
    source = { branch = 'sigma-2.6' },
    { 'build',   { 'kernel',   'build'  } },
    { 'modules'  },
    { 'install', { 'firmware', 'unpack' } }
}

package { 'chicken', 'host' }

firmware_package { 'chicken', 'target',
    { 'build', { 'chicken',  'install', 'host' } }
}

package { 'chicken-eggs', 'host',
    { 'install', { 'chicken', 'install', 'host' } }
}

package { 'chicken-eggs', 'target',
    { 'install',
        { 'toolchain'                         },
        { 'chicken',      'install', 'target' },
        { 'chicken-eggs', 'install', 'host'   },
        { 'dbus',         'install'           },
        { 'sqlite',       'install', 'target' },
    }
}

package { 'libuv', 'host' }

firmware_package { 'libuv', 'target' }

firmware_package { 'dbus',
    { 'build', { 'expat', 'install', 'target' } }
}

firmware_package { 'expat', 'target' }

firmware_package { 'freetype', 'target' }

firmware_package { 'rsync', 'target' }

firmware_package { 'sqlite', 'target' }

firmware_package { 'libnl', 'target' }

firmware_package { 'wpa_supplicant',
    { 'build',
        { 'dbus',  'install'           },
        { 'libnl', 'install', 'target' },
    }
}

firmware_package { 'zlib' }

firmware_package { 'libpng', 'target',
    { 'build', { 'zlib', 'install' } }
}

firmware_package { 'xtables', 'target' }

firmware_package { 'xtables-addons', 'target',
    { 'build', { 'xtables', 'install', 'target' } }
}

package { 'ffmpeg', 'host',
    { 'build', { 'ast-files', 'unpack' } }
}

firmware_package { 'ffmpeg', 'target',
    { 'build', { 'ast-files', 'unpack' } }
}

firmware_package { 'soundtouch' }

package { 'astindex',
    { 'unpack', { 'karaoke-player', 'unpack' } }
}

package { 'karaoke-player', 'host',
    { 'build',
        { 'astindex',     'unpack'          },
        { 'chicken-eggs', 'install', 'host' },
        { 'ffmpeg',       'install', 'host' },
        { 'libuv',        'install', 'host' },
    }
}

firmware_package { 'karaoke-player', 'target',
    { 'build',
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
    { 'install',
        { 'chicken-eggs', 'install', 'target' }
    }
}

package { 'firmware',
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

firmware_package { 'libffi', 'target' }

firmware_package { 'glib',
    { 'build',
        { 'zlib',     'install' },
        { 'libffi',   'install', 'target' },
    }
}

firmware_package { 'connman',
    { 'build',
        { 'dbus',           'install'           },
        { 'glib',           'install'           },
        { 'xtables-addons', 'install', 'target' },
    }
}

firmware_package { 'fribidi', 'target',
    { 'build', { 'glib', 'install' } }
}

firmware_package { 'libass', 'target',
    { 'build',
        { 'freetype', 'install', 'target' },
        { 'fribidi',  'install', 'target' },
    }
}
