rule {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/sigma-kernel.git',
        branch   = 'sigma-2.6'
    },
    build = {
        type = 'Kbuild',
        in_source = true
    }
}
