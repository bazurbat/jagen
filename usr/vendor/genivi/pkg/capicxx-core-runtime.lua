return {
    source = {
        type = 'git',
        location = 'https://github.com/GENIVI/capicxx-core-runtime.git'
    },
    build = {
        type = 'CMake',
        options = {
            '-DUSE_CONSOLE=ON',
            '-DUSE_FILE=ON',
        }
    }
}
