package {
    name   = 'libass',
    build  = {
        type    = 'GNU',
        options = '--disable-static --disable-enca '..
                  '--enable-fontconfig --disable-harfbuzz'
    },
    source = 'libass-0.12.2.tar.xz'
}
