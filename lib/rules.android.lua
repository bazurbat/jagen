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

package { 'chicken', 'host' }

package { 'firmware', 'target',
    { 'install',
        needs = {
            'libuv'
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
