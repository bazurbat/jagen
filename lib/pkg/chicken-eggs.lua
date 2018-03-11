return {
    source = {
        type     = 'git',
        location = 'https://github.com/bazurbat/chicken-eggs.git',
    },
    build = {
        type = 'cmake'
    },
    requires = { 'chicken' }
}
