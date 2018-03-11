return {
    source = {
        type      = 'dist',
        location  = 'https://www.gnupg.org/ftp/gcrypt/libgpg-error/libgpg-error-1.17.tar.bz2',
        sha256sum = '3ff4e5a71116eb862cd14185fcd282850927b8608e3b4186834fd940fbef57b5'
    },
    build  = {
        type = 'gnu',
        options = {
            '--disable-rpath',
            '--disable-languages'
        },
    },
    install = {
        config_script = '/bin/gpg-error-config',
        libs = { 'gpg-error' }
    }
}
