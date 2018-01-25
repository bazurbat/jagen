return {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/hi-kernel.git',
        branch   = 'master'
    },
    build = {
        type = 'linux_kernel',
        in_source = true
    },
    export = {
        env = {
            KERNEL_SRC = '$pkg_source_dir'
        }
    }
}
