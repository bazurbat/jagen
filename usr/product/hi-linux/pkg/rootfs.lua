rule {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/rootfs.git',
        branch   = 'master'
    },
    build = {
        dir = '$jagen_target_dir'
    }
}
