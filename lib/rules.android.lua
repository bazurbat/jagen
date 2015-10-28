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

package { 'android-cmake' }

package { 'libtool', 'host' }

package { 'chicken', 'host',
    source = { branch = 'stable-cmake' }
}

package { 'chicken-eggs', 'host',
    source = { branch = 'newchicken' },
    { 'install',
        needs = { 'chicken' }
    }
}

package { 'firmware', 'target',
    { 'install',
        needs = {
            'chicken',
            'chicken-eggs',
            'ffmpeg',
            'libuv',
        }
    },
    { 'strip' },
    { 'deploy' }
}

firmware_package { 'libuv',
    build  = {
        options = '--disable-static'
    }
}

firmware_package { 'ffmpeg' }

firmware_package { 'chicken',
    source = {
        branch = 'stable-cmake'
    },
    { 'build', { 'chicken', 'install', 'host' } }
}

firmware_package { 'chicken-eggs',
    source = { branch = 'newchicken' },
    { 'install',
        needs = {
            'chicken',
            'sqlite'
        }
    }
}
