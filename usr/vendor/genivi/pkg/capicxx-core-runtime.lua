return {
    source = {
        type = 'git',
        location = 'https://github.com/GENIVI/capicxx-core-runtime.git'
    },
    patches = {
        { 'capicxx-core-runtime-Add-USE_DLT-option-to-CMakeLists', 1 }
    },
    build = {
        type = 'CMake',
        options = {
            '-DUSE_CONSOLE=ON',
            '-DUSE_FILE=ON',
        }
    }
}
