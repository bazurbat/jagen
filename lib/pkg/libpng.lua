package {
    name   = 'libpng',
    source = 'libpng-1.6.17.tar.xz',
    build  = {
        type = 'GNU',
        libs = { 'png16' }
    },
    requires = { 'zlib' }
}
