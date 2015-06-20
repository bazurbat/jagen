local packages = {}

local function package(rule, stages)
    table.insert(packages, jagen.read(rule, stages))
end

local function rootfs_package(rule)
    local stages = {
        { 'build', { 'rootfs', 'build' } },
        { 'install' }
    }
    package(rule, stages)
end

local function kernel_package(rule)
    local stages = {
        { 'build', { 'kernel', 'build' } },
        { 'install' }
    }
    package(rule, stages)
end

local function firmware_package(rule)
    local stages = {
        { 'build' },
        { 'install', { 'firmware', 'unpack' } }
    }
    package(rule, stages)
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
    { 'install', 'host' },
    { 'build',   'target',
        { 'gpgme', 'install' },
        { 'dbus',  'install' }
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
        { 'xsdk',       'unpack'            },
        { 'make',       'install', 'host'   }
    },
    { 'install',
        { 'kernel',     'install'           },
        { 'busybox',    'install'           },
        { 'gnupg',      'install'           },
        { 'loop-aes',   'install'           },
        { 'mrua',       'modules'           },
        { 'ntpclient',  'install'           },
        { 'ralink',     'install'           },
        { 'util-linux', 'install'           },
        { 'utils',      'install', 'target' }
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
    name = 'util-linux',
    { 'patch', { 'libtool', 'install' } }
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
        { 'linux',  'unpack' },
        { 'ezboot', 'build'  },
        { 'rootfs', 'build'  }
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
    source = {
        branch = 'cmake'
    },
    { 'build',   'host' },
    { 'install', 'host' },
    { 'build',   'target',
        { 'chicken',  'install', 'host' }
    },
    { 'install', 'target',
        { 'firmware', 'unpack'          }
    }
}

package {
    name = 'chicken-eggs',
    source = {
        branch = 'master'
    },
    { 'install', 'host',
        { 'chicken', 'install', 'host'        }
    },
    { 'install', 'target',
        { 'chicken',      'install', 'target' },
        { 'sqlite',       'install'           },
        { 'chicken-eggs', 'install', 'host'   },
        { 'dbus',         'install'           }
    }
}

package {
    name = 'libuv',
    { 'build',   'host' },
    { 'install', 'host' },
    { 'build',   'target' },
    { 'install', 'target',
        { 'firmware', 'unpack' }
    }
}

firmware_package {
    name = 'dbus',
    { 'build', { 'expat', 'install' } }
}

firmware_package {
    name = 'expat'
}

firmware_package {
    name = 'freetype'
}

firmware_package {
    name = 'rsync'
}

firmware_package {
    name = 'sqlite',
    { 'patch', { 'libtool', 'install' } }
}

firmware_package {
    name = 'libnl',
    { 'build'   },
    { 'install' }
}

firmware_package {
    name = 'wpa_supplicant',
    { 'build',
        { 'dbus',  'install' },
        { 'libnl', 'install' }
    }
}

firmware_package {
    name = 'zlib'
}

firmware_package {
    name = 'libpng',
    { 'build',
        { 'zlib', 'install' }
    }
}

firmware_package {
    name = 'xtables'
}

firmware_package {
    name = 'xtables-addons',
    { 'build', { 'xtables', 'install' } }
}

package {
    name = 'ffmpeg',
    { 'build',   'host',
        { 'ast-files', 'unpack' }
    },
    { 'install', 'host' },
    { 'build',   'target',
        { 'ast-files', 'unpack' }
    },
    { 'install', 'target',
        { 'firmware',  'unpack' }
    }
}

package {
    name = 'soundtouch',
    { 'build' },
    { 'install', { 'firmware', 'unpack' } }
}

package {
    name = 'astindex',
    { 'unpack', { 'karaoke-player', 'unpack' } }
}

package {
    name = 'karaoke-player',
    { 'build', 'host',
        { 'astindex',     'unpack'            },
        { 'ffmpeg',       'install', 'host'   },
        { 'chicken-eggs', 'install', 'host'   },
        { 'libuv',        'install', 'host'   }
    },
    { 'install', 'host'                       },
    { 'build',   'target',
        { 'astindex',     'unpack'            },
        { 'chicken',      'install', 'target' },
        { 'chicken-eggs', 'install', 'host'   },
        { 'dbus',         'install'           },
        { 'ffmpeg',       'install', 'target' },
        { 'freetype',     'install'           },
        { 'libuv',        'install', 'target' },
        { 'libpng',       'install'           },
        { 'libass',       'install'           },
        { 'mrua',         'build'             },
        { 'soundtouch',   'install'           },
        { 'connman',      'install'           }
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
        { 'ezboot',         'install'           },
        { 'mrua',           'install'           },
        { 'kernel',         'image'             },
        { 'karaoke-player', 'install', 'target' },
        { 'dbus',           'install'           },
        { 'expat',          'install'           },
        { 'freetype',       'install'           },
        { 'rsync',          'install'           },
        { 'sqlite',         'install'           },
        { 'wpa_supplicant', 'install'           },
        { 'zlib',           'install'           },
        { 'libpng',         'install'           },
        { 'libffi',         'install'           },
        { 'glib',           'install'           },
        { 'connman',        'install'           }
    },
    { 'strip' }
}

package {
    name = 'libffi',
    { 'build',   { 'libtool',  'install' } },
    { 'install', { 'firmware', 'unpack'  } }
}

package {
    name = 'glib',
    { 'patch',
        { 'libtool',  'install' }
    },
    { 'build',
        { 'zlib',     'install' },
        { 'libffi',   'install' }
    },
    { 'install',
        { 'firmware', 'unpack'  }
    }
}

package {
    name = 'connman',
    { 'build',
        { 'libtool',        'install' },
        { 'dbus',           'install' },
        { 'glib',           'install' },
        { 'xtables-addons', 'install' }
    },
    { 'install',
        { 'firmware',       'unpack'  }
    }
}

package {
    name = 'fribidi',
    { 'build' },
    { 'install',
        { 'firmware', 'unpack' }
    }
}

package {
    name = 'fontconfig',
    { 'build' },
    { 'install',
        { 'firmware', 'unpack' }
    }
}

package {
    name = 'libass',
    { 'build',
        { 'fribidi', 'install' },
        { 'fontconfig', 'install' }
    },
    { 'install',
        { 'firmware', 'unpack' }
    }
}

return packages
