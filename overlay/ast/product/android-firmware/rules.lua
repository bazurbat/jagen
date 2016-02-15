-- Android rules

package { 'android-cmake' }

package { 'cmake-modules' }

package { 'chicken', 'host',
    { 'configure',
        { 'android-cmake', 'unpack' }
    }
}

package { 'chicken-eggs', 'host',
    { 'install',
        { 'chicken', 'install', 'host' }
    }
}

local firmware_package_template = {
    config = 'target',
    { 'install', { 'firmware', 'unpack' } }
}

local function firmware_package(rule)
    package(rule, firmware_package_template)
end

package { 'firmware',
    { 'compile',
        { 'hi-utils',       'install', 'target' },
        { 'chicken',        'install', 'target' },
        { 'chicken-eggs',   'install', 'target' },
        { 'ffmpeg',         'install', 'target' },
        { 'karaoke-player', 'install', 'target' },
        { 'libuv',          'install', 'target' },
    },
    { 'install' }
}

firmware_package { 'hi-utils',
    { 'configure',
        { 'android-cmake', 'unpack' },
        { 'cmake-modules', 'unpack' },
    }
}

firmware_package { 'chicken',
    { 'configure',
        { 'chicken', 'install', 'host' }
    }
}

firmware_package { 'chicken-eggs',
    { 'install',
        { 'chicken-eggs', 'install', 'host'   },
        { 'chicken',      'install', 'target' },
        { 'sqlite',       'install', 'target' },
    }
}

firmware_package { 'sqlite' }

firmware_package { 'ffmpeg' }

firmware_package { 'karaoke-player',
    { 'configure',
        { 'astindex',      'unpack'            },
        { 'cmake-modules', 'unpack'            },
        { 'chicken-eggs',  'install', 'host'   },
        { 'chicken-eggs',  'install', 'target' },
        { 'ffmpeg',        'install', 'target' },
        { 'libuv',         'install', 'target' },
    },
    { 'install' }
}

package { 'astindex',
    { 'unpack', { 'karaoke-player', 'unpack' } }
}

firmware_package { 'libuv',
    build  = {
        options = { '--disable-static' }
    }
}
