return {
    source = {
        type      = 'dist',
        location  = 'https://github.com/libexpat/libexpat/releases/download/R_2_2_5/expat-2.2.5.tar.bz2',
        sha256sum = 'd9dc32efba7e74f788fcc4f212a43216fc37cf5f23f4c2339664d473353aedf6'
    },
    build  = {
        type = 'gnu',
    },
    install = {
        libs = { 'expat' }
    }
}
