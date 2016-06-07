return {
    source = 'loop-AES-v3.7b.tar.bz2',
    build  = {
        type = 'make',
        in_source = true
    },
    install = {
        type = 'none',
        modules = 'tmp-d-kbuild'
    }
}
