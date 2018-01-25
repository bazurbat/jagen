return {
    source = {
        type     = 'hg',
        location = 'ssh://hg@bitbucket.org/art-system/karaoke-player',
        branch   = 'master'
    },
    build = {
        type = 'CMake'
    },
    requires = {
        'cmake-modules'
    },
    use = 'hi-sdk'
}
