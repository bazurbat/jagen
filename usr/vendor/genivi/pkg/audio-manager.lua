return {
    source = {
        type     = 'git',
        location = 'https://github.com/GENIVI/AudioManager.git',
        tag      = '7.5',
    },
    patches = {
        { 'audio-manager-pass-all-LDFLAGS-to-linker-when-building-wrappers', 1 }
    },
    build = {
        type = 'cmake',
        options = {
            '-DWITH_TESTS=OFF',
            '-DWITH_DOCUMENTATION=OFF',
            '-DWITH_DLT=OFF',
            '-DWITH_TELNET=OFF',
            '-DWITH_SYSTEMD_WATCHDOG=OFF',
            '-DGLIB_DBUS_TYPES_TOLERANT=ON',
            '-DWITH_CAPI_WRAPPER=ON',
            '-DWITH_DBUS_WRAPPER=ON',
            '-DWITH_SHARED_UTILITIES=ON',
            '-DWITH_SHARED_CORE=ON',
        }
    },
    requires = {
        'capicxx-core-runtime',
        'dbus',
    }
}
