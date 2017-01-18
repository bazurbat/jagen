return {
    source = {
        type = 'git',
        location = 'https://github.com/GENIVI/capicxx-dbus-runtime.git'
    },
    build = {
        type = 'CMake'
    },
    requires = {
        'capicxx-core-runtime',
        'dbus'
    }
}
