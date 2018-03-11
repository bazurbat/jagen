return {
    source = {
        type      = 'dist',
        location  = 'ftp://ftp.simplesystems.org/pub/libpng/png/src/libpng16/libpng-1.6.26.tar.xz',
        sha256sum = '266743a326986c3dbcee9d89b640595f6b16a293fd02b37d8c91348d317b73f9'
    },
    build  = {
        type = 'gnu',
    },
    install = {
        libs = { 'png16' }
    },
    requires = { 'zlib' }
}
