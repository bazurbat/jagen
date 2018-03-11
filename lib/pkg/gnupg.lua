return {
    source = {
        type      = 'dist',
        location  = 'https://www.gnupg.org/ftp/gcrypt/gnupg/gnupg-1.4.18.tar.bz2',
        sha256sum = 'b7b5fdda78849955e0cdbc5a085f3a08f8b7fba126c622085debb62def5d6388'
    },
    build  = {
        type = 'gnu',
        options = {
            '--enable-minimal',
            '--disable-card-support',
            '--disable-agent-support',
            '--enable-rsa',
            '--disable-idea',
            '--enable-cast5',
            '--disable-blowfish',
            '--disable-aes',
            '--disable-twofish',
            '--disable-camellia',
            '--enable-sha256',
            '--disable-sha512',
            '--disable-bzip2',
            '--disable-exec',
            '--disable-photo-viewers',
            '--disable-keyserver-helpers',
            '--disable-ldap',
            '--disable-hkp',
            '--disable-finger',
            '--disable-generic',
            '--disable-mailto',
            '--disable-keyserver-path',
            '--disable-dns-srv',
            '--disable-dns-pka',
            '--disable-dns-cert',
            '--disable-regex'
        }
    }
}
