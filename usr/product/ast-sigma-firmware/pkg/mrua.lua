return {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/sigma-mrua.git',
        branch   = '3.11.3'
    },
    build = {
        type = true,
        in_source = true,
        kernel_modules = true,
        jobs = 1
    },
    install = {
        module_dirs = {
            -- order matters to avoid warnings from depmod
            'MRUA_src/llad/direct/kernel_src',
            'MRUA_src/rua/emhwlib_kernel/kernel_src',
        }
    },
    use = { 'kernel', 'rootfs' }
}
