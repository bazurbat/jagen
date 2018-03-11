return {
    source = {
        type     = 'dist',
        location = 'https://www.sqlite.org/2016/sqlite-autoconf-3150200.tar.gz',
        sha1sum  = '31f52169bcfeef9efb61480d0950e928ad059552'
    },
    patches = {
        { 'sqlite-3.8.1-autoconf-dlopen_check', 0 }
    },
    build = {
        type    = 'gnu',
        options = { '--disable-static' },
        libs    = { 'sqlite3' },
        autoreconf = true,
    },
}
