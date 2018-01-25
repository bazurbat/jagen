return {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/rootfs.git',
        branch   = 'master'
    },
    build = {
        type = true,
        dir = '$jagen_target_dir'
    },
    use = 'hi-sdk'
}
