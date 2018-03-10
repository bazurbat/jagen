return {
    source = {
        type     = 'git',
        location = 'https://github.com/GENIVI/capicxx-dbus-runtime.git',
        tag      = '3.1.12'
    },
    build = {
        type = 'CMake',
        options = {
            '-DUSE_CONSOLE=ON',
            '-DUSE_FILE=ON',
        }
    },
    requires = {
        'capicxx-core-runtime',
        'dbus'
    }
}
