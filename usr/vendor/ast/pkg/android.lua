return {
    source = {
        type     = 'repo',
        location = 'git@bitbucket.org:art-system/hia-manifest.git',
        branch   = 'master',
        dir      = 'android'
    },
    build = {
        type = true,
        in_source = true
    }
}
