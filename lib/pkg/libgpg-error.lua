return {
    source = 'libgpg-error-1.17.tar.bz2',
    build  = {
        type = 'GNU',
        options = { '--disable-rpath', '--disable-languages' },
        libs = { 'gpg-error' }
    },
    install = {
        config_script = '/bin/gpg-error-config'
    }
}
