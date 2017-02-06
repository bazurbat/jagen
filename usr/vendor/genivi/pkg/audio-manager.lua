return {
    source = {
        type = 'git',
        location = 'https://github.com/GENIVI/AudioManager.git',
        branch = '7.5',
        ignore_dirty = true
    },
    patches = {
        { 'audio-manager-pass-all-LDFLAGS-of-the-found-CommonAPI-to-the-build', 1 }
    },
    build = {
        type = 'CMake',
        options = {
            '-DWITH_TESTS=OFF',
            '-DWITH_DOCUMENTATION=OFF',
            '-DWITH_DLT=OFF',
            '-DWITH_TELNET=OFF',
            '-DWITH_SYSTEMD_WATCHDOG=OFF',
            '-DGLIB_DBUS_TYPES_TOLERANT=ON',
            '-DWITH_CAPI_WRAPPER=ON',
            '-DWITH_DBUS_WRAPPER=OFF',
            '-DWITH_SHARED_UTILITIES=ON',
            '-DWITH_SHARED_CORE=ON',
        }
    },
    requires = {
        'capicxx-core-runtime'
    }
}
