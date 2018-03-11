return {
    source = {
        type      = 'dist',
        location  = 'https://www.gnupg.org/ftp/gcrypt/gpgme/gpgme-1.5.1.tar.bz2',
        sha256sum = '6c2f3af8ceeb303277d44ec0216d9a1271701f65b91d5942264cf7cefb4a11e3'
    },
    build  = {
        type = 'gnu',
        options = {
            '--includedir=/include',
            '--disable-glibtest',
            '--enable-fixed-path=/bin',
            '--disable-gpgconf-test',
            '--disable-gpg-test',
            '--disable-gpgsm-test',
            '--disable-g13-test',
            '--with-libgpg-error-prefix=$pkg_install_root',
            '--with-libassuan-prefix=$pkg_install_root'
        },
    },
    install = {
        libs = { 'gpgme' }
    },
    requires = { 'libassuan' }
}
