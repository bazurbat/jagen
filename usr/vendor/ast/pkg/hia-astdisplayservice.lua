return {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/hia-astdisplayservice.git',
        branch   = 'master'
    },
    build = {
        type = 'CMake'
    },
    requires = {
        'karaoke-player'
    }
}
