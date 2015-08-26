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
