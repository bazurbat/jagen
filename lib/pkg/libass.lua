package {
    name   = 'libass',
    build  = {
        type    = 'GNU',
        options = '--disable-static --disable-enca '..
                  '--enable-fontconfig --disable-harfbuzz',
        libs    = { 'ass' }
    },
    source = 'libass-0.12.2.tar.xz'
}
