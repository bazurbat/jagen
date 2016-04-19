rule {
    source = 'sqlite-autoconf-3080403.tar.gz',
    patches = {
        { 'sqlite-3.8.1-autoconf-dlopen_check', 0 }
    },
    build = {
        type    = 'GNU',
        options = { '--disable-static' },
        libs    = { 'sqlite3' },
        autoreconf = true,
    },
}
