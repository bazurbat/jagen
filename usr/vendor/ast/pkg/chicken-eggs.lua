return {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/chicken-eggs.git',
        branch   = 'release'
    },
    build = {
        type = 'CMake'
    },
    requires = { 'chicken' }
}
