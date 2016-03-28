package {
    source = 'libassuan-2.1.2.tar.bz2',
    build  = {
        type = 'GNU',
        options = {
            '--with-libgpg-error-prefix=$pkg_sysroot'
        },
        libs = { 'assuan' }
    },
    requires = { 'libgpg-error' }
}
