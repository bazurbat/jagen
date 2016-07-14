return {
    source = {
        type      = 'dist',
        location  = 'ftp://ftp.simplesystems.org/pub/libpng/png/src/libpng16/libpng-1.6.17.tar.xz',
        sha256sum = '98507b55fbe5cd43c51981f2924e4671fd81fe35d52dc53357e20f2c77fa5dfd'
    },
    build  = {
        type = 'GNU',
        libs = { 'png16' }
    },
    requires = { 'zlib' }
}
