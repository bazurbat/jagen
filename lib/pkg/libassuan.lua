return {
    source = {
        type      = 'dist',
        location  = 'https://www.gnupg.org/ftp/gcrypt/libassuan/libassuan-2.1.2.tar.bz2',
        sha256sum = '39f8a7c9349aaaf7ccd937b90660153ec4d2d4df2465018754e5bcae5b1db77b'
    },
    build  = {
        type = 'gnu',
        options = {
            '--with-libgpg-error-prefix=$pkg_install_root'
        },
    },
    install = {
        libs = { 'assuan' }
    },
    requires = { 'libgpg-error' }
}
