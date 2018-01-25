return {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/sigma-kernel.git',
        branch   = 'sigma-2.6'
    },
    build = {
        type = 'Kbuild',
        in_source = true
    },
    env = {
        LINUX_KERNEL = '$pkg_source_dir/linux'
    },
    use = { 'ezboot', 'rootfs', 'xsdk' },
    export = {
        env = {
            -- for external modules
            KERNEL_SRC     = '$pkg_env_LINUX_KERNEL',
            -- for mrua
            UCLINUX_KERNEL = '$pkg_env_LINUX_KERNEL',
        }
    }
}
