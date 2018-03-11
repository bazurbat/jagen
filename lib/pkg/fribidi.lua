return {
    source = {
        type      = 'dist',
        location  = 'https://www.fribidi.org/download/fribidi-0.19.6.tar.bz2',
        sha256sum = 'cba8b7423c817e5adf50d28ec9079d14eafcec9127b9e8c8f1960c5ad585e17d'
    },
    patches = {
        { 'fribidi-0.19.2-nodoc',            0 },
        { 'fribidi-0.19.6-page-size-header', 1 }
    },
    build = {
        type    = 'gnu',
        options = { '--with-glib=yes' },
        libs    = { 'fribidi' },
        autoreconf = true
    },
    requires = { 'glib' }
}
