-- Sigma rules

local function rootfs_package(rule)
    rule.config = 'target'
    table.insert(rule, { 'build', { 'rootfs', 'build' } })
    package(rule)
end

-- base

package { 'ast-files' }

package { 'linux',
    source = { branch = 'ast50' }
}

package { 'xsdk' }

package { 'ucode', 'target',
    { 'unpack',  { 'mrua',     'build'  } },
    { 'install', { 'firmware', 'unpack' } }
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

local function kernel_package(rule)
    rule.config = 'target'
    table.insert(rule, { 'build', { 'kernel', 'build' } })
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

rootfs_package { 'utils',
    { 'build',
        { 'dbus',  'install', 'target' },
        { 'gpgme', 'install', 'target' }
    }
}

rootfs_package { 'libgpg-error' }

rootfs_package { 'libassuan',
    { 'build',
        { 'libgpg-error', 'install', 'target' }
    }
}

rootfs_package { 'gpgme',
    { 'build',
        { 'libassuan', 'install', 'target' }
    }
}

-- firmware

local function firmware_package(rule)
    rule.config = 'target'
    table.insert(rule, { 'install', { 'firmware', 'unpack' } })
    package(rule)
end

package { 'firmware',
    { 'material',
        { 'mrua', 'build' }
    },
    { 'install',
        { 'ezboot',         'install', 'target' },
        { 'karaoke-player', 'install', 'target' },
        { 'kernel',         'image'             },
        { 'mrua',           'install'           },
        { 'rsync',          'install', 'target' },
        { 'wpa_supplicant', 'install', 'target' },
    },
    { 'strip' }
}

firmware_package { 'karaoke-player',
    { 'build',
        { 'astindex',     'unpack'            },
        { 'mrua',         'build',            },
        { 'chicken-eggs', 'install', 'host'   },
        { 'chicken-eggs', 'install', 'target' },
        { 'connman',      'install', 'target' },
        { 'dbus',         'install', 'target' },
        { 'ffmpeg',       'install', 'target' },
        { 'freetype',     'install', 'target' },
        { 'libass',       'install', 'target' },
        { 'libpng',       'install', 'target' },
        { 'libuv',        'install', 'target' },
        { 'soundtouch',   'install', 'target' },
    }
}

firmware_package { 'chicken',
    { 'build', { 'chicken',  'install', 'host' } }
}

firmware_package { 'chicken-eggs',
    { 'install',
        { 'chicken',      'install', 'target' },
        { 'chicken-eggs', 'install', 'host'   },
        { 'dbus',         'install', 'target' },
        { 'sqlite',       'install', 'target' },
    }
}

firmware_package { 'dbus',
    { 'build',
        { 'expat', 'install', 'target' }
    }
}

firmware_package { 'expat' }

firmware_package { 'sqlite' }

firmware_package { 'connman',
    { 'build',
        { 'dbus',           'install', 'target' },
        { 'glib',           'install', 'target' },
        { 'xtables-addons', 'install', 'target' },
    }
}

firmware_package { 'glib',
    { 'build',
        { 'libffi', 'install', 'target' },
        { 'zlib',   'install', 'target' },
    }
}

firmware_package { 'libffi' }

firmware_package { 'zlib' }

firmware_package { 'xtables-addons',
    { 'build',
        { 'xtables', 'install', 'target' }
    }
}

firmware_package { 'xtables' }

firmware_package { 'ffmpeg' }

firmware_package { 'freetype' }

firmware_package { 'libass',
    { 'build',
        { 'freetype', 'install', 'target' },
        { 'fribidi',  'install', 'target' },
    }
}

firmware_package { 'fribidi',
    { 'build',
        { 'glib', 'install', 'target' }
    }
}

firmware_package { 'libpng',
    { 'build',
        { 'zlib', 'install', 'target' }
    }
}

firmware_package { 'libuv' }

firmware_package { 'soundtouch' }

firmware_package { 'rsync' }

firmware_package { 'wpa_supplicant',
    { 'build',
        { 'dbus', 'install', 'target' }
    }
}
