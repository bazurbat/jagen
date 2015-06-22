package {
    name   = 'fribidi',
    build  = {
        type    = 'GNU',
        options = '--with-glib=no',
        libs    = { 'fribidi' }
    },
    source = 'fribidi-0.19.6.tar.bz2'
}
