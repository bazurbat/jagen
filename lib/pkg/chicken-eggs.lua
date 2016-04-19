rule {
    source = {
        type     = 'git',
        location = 'https://github.com/bazurbat/chicken-eggs.git',
        branch   = 'master'
    },
    build = {
        type = 'CMake'
    },
    requires = { 'chicken' }
}
