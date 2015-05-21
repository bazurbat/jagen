local packages = {}

local function package(pkg, stages)
    table.insert(packages, Jagen.package(pkg, stages))
end

local function rootfs_package(pkg)
    local stages = {
        { 'build', { 'rootfs', 'build' } },
        { 'install' }
    }
    package(pkg, stages)
end

local function kernel_package(pkg)
    local stages = {
        { 'build', { 'kernel', 'build' } },
        { 'install' }
    }
    package(pkg, stages)
end

local function firmware_package(pkg)
    local stages = {
        { 'build' },
        { 'install', { 'firmware', 'unpack' } }
    }
    package(pkg, stages)
end

-- base

package {
    name = 'ast-files',
    source = 'git git@bitbucket.org:art-system/files.git'
}

package {
    name = 'linux',
    source = 'git git@bitbucket.org:art-system/linux.git',
    branch = pkg_flag 'new_kernel' and 'sigma-3.4' or 'ast50'
}

package {
    name = 'xsdk',
    source = 'dist ${cpukeys}.tar.gz'
}

package {
    name = 'ucode',
    source = {
        [308] = 'dist mruafw_SMP8654F_prod_3_8_3.tgz',
        [309] = 'dist mruafw_SMP8654F_prod_3_9_2.tgz',
        [311] = 'dist mruafw_SMP8654F_3_11_3_prod.tgz'
    },
    { 'unpack',  { 'mrua',     'build'  } },
    { 'install', { 'firmware', 'unpack' } }
}

-- tools

package {
    name = 'make',
    source = 'dist make-3.80.tar.bz2',
    { 'build',   'host' },
    { 'install', 'host' }
}

-- host

---[[

package {
    name = 'libtool',
    source = 'dist libtool-2.4.3.tar.xz',
    patches = {
        { 'libtool-2.4.3-no-clean-gnulib',   1 },
        { 'libtool-2.4.3-test-cmdline_wrap', 1 }
    },
    { 'build'   },
    { 'install' }
}

-- utils

package {
    name = 'utils',
    source = 'git git@bitbucket.org:art-system/sigma-utils.git',
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
        type = 'git',
        location = 'git@bitbucket.org:art-system/sigma-ezboot.git',
        branch = "sdk4"
    }
}

-- debugging

-- (when (string=? "Debug" (env 'build-type))
package {
    name = 'gdb',
    source = 'dist gdb-7.9.tar.xz',
    { 'build',   'host' },
    { 'install', 'host' }
}

rootfs_package {
    name = 'gdbserver',
    source = 'dist gdb-7.9.tar.xz'
}

rootfs_package {
    name = 'strace',
    source = 'dist strace-4.8.tar.xz'
}

-- rootfs

package {
    name = 'rootfs',
    source = 'git git@bitbucket.org:art-system/sigma-rootfs.git',
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
    source = 'dist busybox-1.22.1.tar.bz2',
    { 'patch', { 'ast-files', 'unpack' } }
}

rootfs_package {
    name = 'ntpclient',
    source = 'dist ntpclient-2010.tar.gz'
}

rootfs_package {
    name = 'util-linux',
    source = 'dist util-linux-2.23.2.tar.xz',
    patches = {
        { 'util-linux-2.23.2', 1 }
    },
    { 'patch', { 'libtool', 'install' } }
}

-- gpgme

rootfs_package {
    name = 'libgpg-error',
    source = 'dist libgpg-error-1.17.tar.bz2'
}

rootfs_package {
    name = 'libassuan',
    source = 'dist libassuan-2.1.2.tar.bz2',
    { 'build', { 'libgpg-error', 'install' } }
}

rootfs_package {
    name = 'gpgme',
    source = 'dist gpgme-1.5.1.tar.bz2',
    { 'build', { 'libassuan', 'install' } }
}

rootfs_package {
    name = 'gnupg',
    source = 'dist gnupg-1.4.18.tar.bz2'
}

-- kernel

package {
    name = 'kernel',
    source = {
        'git',
        'git@bitbucket.org:art-system/sigma-kernel.git',
        'sigma-2.6'
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
    name = 'ralink',
    source = 'dist DPO_RT5572_LinuxSTA_2.6.1.3_20121022.tar.bz2',
    patches = {
        { 'DPO_RT5572_LinuxSTA_2.6.1.3_20121022-no-tftpboot', 1 },
        { 'DPO_RT5572_LinuxSTA_2.6.1.3_20121022-encrypt',     1 }
    }
}

kernel_package {
    name = 'loop-aes',
    source = 'dist loop-AES-v3.7b.tar.bz2'
}

package {
    name = 'mrua',
    source = {
        'git',
        'git@bitbucket.org:art-system/sigma-mrua.git',
        'sigma-2.6'
    },
    { 'build',   { 'kernel',   'build'  } },
    { 'modules'  },
    { 'install', { 'firmware', 'unpack' } }
}

package {
    name = 'chicken',
    source = {
        'git',
        'https://github.com/bazurbat/chicken-scheme.git',
        'cmake'
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
        'git',
        'https://github.com/bazurbat/chicken-eggs.git',
        'master'
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

firmware_package {
    name = 'dbus',
    source = 'dist dbus-1.6.18.tar.gz',
    { 'build', { 'expat', 'install' } }
}

firmware_package {
    name = 'expat',
    source = 'dist expat-2.1.0.tar.gz'
}

firmware_package {
    name = 'freetype',
    source = 'dist freetype-2.5.0.1.tar.bz2',
    patches = {
        { 'freetype-2.3.2-enable-valid',   1 },
        { 'freetype-2.4.11-sizeof-types',  1 },
        { 'freetype-2.4.12-clean-include', 1 }
    }
}

firmware_package {
    name = 'libuv',
    source = 'dist libuv-v0.10.25.tar.gz'
}

firmware_package {
    name = 'rsync',
    source = 'dist rsync-3.1.1.tar.gz'
}

firmware_package {
    name = 'sqlite',
    source = 'dist sqlite-autoconf-3080403.tar.gz',
    patches = {
        { 'sqlite-3.8.1-autoconf-dlopen_check', 0 }
    }
}

firmware_package {
    name = 'libnl',
    source = 'dist libnl-3.2.25.tar.gz',
    patches = {
        { 'libnl-3.2.20-rtnl_tc_get_ops', 1 },
        { 'libnl-3.2.20-cache-api',       1 },
    },
    { 'build'   },
    { 'install' }
}

firmware_package {
    name = 'wpa_supplicant',
    source = 'dist wpa_supplicant-2.2.tar.gz',
    patches = {
        { 'wpa_supplicant-2.2-do-not-call-dbus-functions-with-NULL-path', 1 }
    },
    { 'build',
        { 'dbus',  'install' },
        { 'libnl', 'install' }
    }
}

firmware_package {
    name = 'zlib',
    source = 'dist zlib-1.2.8.tar.gz'
}

firmware_package {
    name = 'xtables',
    source = 'dist iptables-1.4.21.tar.bz2'
}

firmware_package {
    name = 'xtables-addons',
    source = 'dist xtables-addons-1.47.1.tar.xz',
    { 'build', { 'xtables', 'install' } }
}

package {
    name = 'ffmpeg',
    source = 'dist ffmpeg-2.2.1.tar.bz2',
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
    source = 'dist soundtouch-1.8.0.tar.gz',
    { 'build' },
    { 'install', { 'firmware', 'unpack' } }
}

package {
    name = 'astindex',
    source = {
        'hg',
        'ssh://hg@bitbucket.org/art-system/astindex',
        directory = 'karaoke-player/source/astindex'
    },
    { 'unpack', { 'karaoke-player', 'unpack' } }
}

package {
    name = 'karaoke-player',
    source = 'hg ssh://hg@bitbucket.org/art-system/karaoke-player',
    { 'build', 'host',
        { 'astindex',     'unpack'            },
        { 'ffmpeg',       'install', 'host'   },
        { 'chicken-eggs', 'install', 'host'   }
    },
    { 'install', 'host' },
    { 'build', 'target',
        { 'astindex',     'unpack'            },
        { 'chicken',      'install', 'target' },
        { 'chicken-eggs', 'install', 'host'   },
        { 'dbus',         'install'           },
        { 'ffmpeg',       'install', 'target' },
        { 'freetype',     'install'           },
        { 'libuv',        'install'           },
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
        { 'libuv',          'install'           },
        { 'rsync',          'install'           },
        { 'sqlite',         'install'           },
        { 'wpa_supplicant', 'install'           },
        { 'zlib',           'install'           },
        { 'libffi',         'install'           },
        { 'glib',           'install'           },
        { 'connman',        'install'           }
    },
    { 'strip' }
}

package {
    name = 'libffi',
    source = 'dist libffi-3.1.tar.gz',
    patches = {
        { 'libffi-3.1-execstack', 0 },
        { 'libffi-3.1-typing_error', 0 },
    },
    { 'build',   { 'libtool',  'install' } },
    { 'install', { 'firmware', 'unpack'  } }
}

package {
    name = 'glib',
    source = 'dist glib-2.40.2.tar.xz',
    patches = {
        { 'glib-2.40.0-external-gdbus-codegen', 1 }
    },
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
    source = 'dist connman-1.28.tar.xz',
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

--]]

return packages
