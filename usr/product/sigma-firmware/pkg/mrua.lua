return {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/sigma-mrua.git',
        branch   = '3.11.3'
    },
    build = {
        type = 'custom',
        in_source = true
    },
    install = {
        modules = {
            -- order matters to avoid warnings from depmod
            'MRUA_src/llad/direct/kernel_src',
            'MRUA_src/rua/emhwlib_kernel/kernel_src',
        }
    }
}
