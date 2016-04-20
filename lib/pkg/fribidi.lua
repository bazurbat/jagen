return {
    source = 'fribidi-0.19.6.tar.bz2',
    patches = {
        { 'fribidi-0.19.2-nodoc',            0 },
        { 'fribidi-0.19.6-page-size-header', 1 }
    },
    build = {
        type    = 'GNU',
        options = { '--with-glib=yes' },
        libs    = { 'fribidi' },
        autoreconf = true
    },
    requires = { 'glib' }
}
