return {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/sigma-kernel.git',
        branch   = 'sigma-2.6'
    },
    build = {
        type = 'kbuild',
        in_source = true,
        release = '2.6.32.15-21-sigma'
    },
    env = {
        LINUX_KERNEL = '$pkg_source_dir/linux'
    },
    use = { 'ezboot', 'rootfs', 'xsdk' },
    export = {
        release = '$pkg_build_release',
        env = {
            -- for external modules
            KERNEL_SRC     = '$LINUX_KERNEL',
            -- for mrua
            UCLINUX_KERNEL = '$LINUX_KERNEL',
        }
    }
}
