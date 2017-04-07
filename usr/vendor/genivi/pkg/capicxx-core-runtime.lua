return {
    source = {
        type     = 'git',
        location = 'https://github.com/GENIVI/capicxx-core-runtime.git',
        branch   = '3.1.8'
    },
    patches = {
        { 'capicxx-core-runtime-Add-USE_DLT-option', 1 }
    },
    build = {
        type = 'CMake',
        options = {
            '-DUSE_CONSOLE=ON',
            '-DUSE_FILE=ON',
        }
    }
}
