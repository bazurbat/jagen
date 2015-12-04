package {
    name   = 'kernel',
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/sigma-kernel.git'
    },
    build = { type = 'Kbuild' }
}
