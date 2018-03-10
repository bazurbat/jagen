return {
    source = {
        type     = 'hg',
        location = 'ssh://hg@bitbucket.org/art-system/karaoke-player',
    },
    build = {
        type = 'CMake'
    },
    requires = {
        'cmake-modules'
    },
    use = 'hi-sdk'
}
