package {
    name   = 'rootfs',
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/sigma-rootfs.git'
    },
    build = {
        type = 'make',
        in_source = true
    }
}
