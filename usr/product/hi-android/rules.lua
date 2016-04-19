-- Android rules

rule { 'android-cmake' }

rule { 'cmake-modules' }

rule { 'chicken', 'host',
    { 'configure',
        { 'android-cmake', 'unpack' }
    }
}

rule { 'chicken-eggs', 'host',
    { 'install',
        { 'chicken', 'install', 'host' }
    }
}

local firmware_rule_template = {
    config = 'target',
    { 'install', { 'firmware', 'unpack' } }
}

local function firmware_rule(r)
    r.template = firmware_rule_template
    rule(r)
end

rule { 'firmware',
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

firmware_rule { 'hi-utils',
    { 'configure',
        { 'android-cmake', 'unpack' },
        { 'cmake-modules', 'unpack' },
    }
}

firmware_rule { 'chicken',
    { 'configure',
        { 'chicken', 'install', 'host' }
    }
}

firmware_rule { 'chicken-eggs',
    { 'install',
        { 'chicken-eggs', 'install', 'host'   },
        { 'chicken',      'install', 'target' },
        { 'sqlite',       'install', 'target' },
    }
}

firmware_rule { 'sqlite' }

firmware_rule { 'ffmpeg' }

firmware_rule { 'karaoke-player',
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

rule { 'astindex',
    { 'unpack', { 'karaoke-player', 'unpack' } }
}

firmware_rule { 'libuv',
    build  = {
        options = { '--disable-static' }
    }
}
