return {
    source = {
        type = 'git',
        location = 'https://github.com/GENIVI/AudioManagerPlugins.git',
    },
    build = {
        type = 'CMake',
        options = {
            '-DWITH_COMMAND_INTERFACE_CAPI=ON',
            '-DWITH_COMMAND_INTERFACE_DBUS=OFF',
            '-DWITH_ROUTING_INTERFACE_ASYNC=OFF',
            '-DWITH_ROUTING_INTERFACE_CAPI=ON',
            '-DWITH_ROUTING_INTERFACE_DBUS=OFF',
            '-DWITH_TEST_CONTROLLER=OFF',
            '-DWITH_GENERIC_CONTROLLER=ON',
        }
    },
    requires = {
        'audio-manager'
    }
}
