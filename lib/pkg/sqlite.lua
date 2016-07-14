return {
    source = {
        type      = 'dist',
        location  = 'https://www.sqlite.org/2016/sqlite-autoconf-3080403.tar.gz',
        sha256sum = 'e0e995e23a324a5d6ae95d8a836240382a4d7475d09707fc469c8cafcbd48d65'
    },
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
