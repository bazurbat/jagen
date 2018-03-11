return {
    source = {
        type     = 'git',
        location = 'https://github.com/GENIVI/capicxx-core-runtime.git',
        tag      = '3.1.12.1'
    },
    build = {
        type = 'cmake',
        options = {
            '-DUSE_CONSOLE=ON',
            '-DUSE_FILE=ON',
        }
    }
}
