package {
    name  = 'sqlite',
    build = {
        type    = 'GNU',
        options = '--disable-static',
        libs    = { 'sqlite3' },
        with_provided_libtool = true,
    },
    source  = 'sqlite-autoconf-3080403.tar.gz',
    patches = {
        { 'sqlite-3.8.1-autoconf-dlopen_check', 0 }
    }
}
