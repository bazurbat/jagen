-- Android rules

package { 'android-cmake' }

package { 'libtool', 'host' }

-- package { 'chicken', 'host',
--     source = { branch = 'stable-cmake' }
-- }

-- package { 'chicken-eggs', 'host',
--     source = { branch = 'newchicken' },
--     { 'install',
--         { 'chicken', 'install', 'host' }
--     }
-- }

local function firmware_package(rule)
    rule.config = 'target'
    table.insert(rule, { 'install', { 'firmware', 'unpack' } })
    package(rule)
end

package { 'firmware',
    { 'install',
        -- { 'chicken',        'install', 'target' },
        -- { 'chicken-eggs',   'install', 'target' },
        { 'ffmpeg',         'install', 'target' },
        -- { 'karaoke-player', 'install', 'target' },
        { 'libuv',          'install', 'target' },
    },
    { 'strip'  },
    { 'deploy' }
}

firmware_package { 'hi-utils' }

-- firmware_package { 'chicken',
--     source = { branch = 'stable-cmake' },
--     { 'build', { 'chicken', 'install', 'host' } }
-- }

-- firmware_package { 'chicken-eggs',
--     source = { branch = 'newchicken' },
--     { 'install',
--         { 'chicken', 'install', 'target' },
--         { 'sqlite',  'install', 'target' },
--     }
-- }

-- firmware_package { 'sqlite' }

firmware_package { 'ffmpeg' }

-- firmware_package { 'karaoke-player',
--     source = { branch = 'newchicken' },
--     { 'build',
--         { 'astindex',     'unpack'            },
--         { 'chicken-eggs', 'install', 'host'   },
--         { 'chicken-eggs', 'install', 'target' },
--         { 'ffmpeg',       'install', 'target' },
--         { 'libuv',        'install', 'target' },
--     },
--     { 'install' }
-- }

package { 'astindex',
    { 'unpack', { 'karaoke-player', 'unpack' } }
}

firmware_package { 'libuv',
    build  = {
        options = '--disable-static'
    }
}
