return {
    source = {
        type     = 'git',
        location = 'https://github.com/GENIVI/capicxx-core-runtime.git',
        branch   = '3.1.12.1'
    },
    build = {
        type = 'CMake',
        options = {
            '-DUSE_CONSOLE=ON',
            '-DUSE_FILE=ON',
        }
    }
}
