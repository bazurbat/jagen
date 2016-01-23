package {
    name   = 'gpgme',
    source = 'gpgme-1.5.1.tar.bz2',
    build  = {
        type = 'GNU',
        options = {
            '--includedir=/include',
            '--disable-glibtest',
            '--enable-fixed-path=/bin',
            '--disable-gpgconf-test',
            '--disable-gpg-test',
            '--disable-gpgsm-test',
            '--disable-g13-test',
            '--with-libgpg-error-prefix=$pkg_dest_dir',
            '--with-libassuan-prefix=$pkg_dest_dir'
        },
        libs = { 'gpgme' }
    },
    requires = { 'libassuan' }
}
