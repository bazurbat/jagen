return {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/hia-astdisplayservice.git',
    },
    build = {
        type = 'cmake'
    },
    requires = {
        'karaoke-player'
    },
    use = 'hi-sdk'
}
