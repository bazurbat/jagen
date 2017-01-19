return {
    source = {
        type = 'git',
        location = 'https://github.com/GENIVI/capicxx-dbus-runtime.git'
    },
    patches = {
        { 'capicxx-dbus-runtime-Add-DBus-LIBDIR-to-link-directories', 1 }
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
