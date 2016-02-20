package {
    source = 'libassuan-2.1.2.tar.bz2',
    build  = {
        type = 'GNU',
        libs = { 'assuan' }
    },
    requires = { 'libgpg-error' }
}
