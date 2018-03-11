return {
    source = {
        type     = 'hg',
        location = 'ssh://hg@bitbucket.org/art-system/karaoke-player',
        branch   = 'stable'
    },
    build = {
        type = 'cmake',
    }
}
