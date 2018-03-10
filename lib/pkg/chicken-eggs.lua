return {
    source = {
        type     = 'git',
        location = 'https://github.com/bazurbat/chicken-eggs.git',
    },
    build = {
        type = 'CMake'
    },
    requires = { 'chicken' }
}
