return {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/hia-astdisplayservice.git',
    },
    build = {
        type = 'CMake'
    },
    requires = {
        'karaoke-player'
    },
    use = 'hi-sdk'
}
