package {
    name   = 'libass',
    source = 'libass-0.12.2.tar.xz',
    build  = {
        type    = 'GNU',
        options = {
            '--disable-static',
            '--disable-enca',
            '--disable-fontconfig',
            '--disable-harfbuzz'
        },
        libs    = { 'ass' }
    },
    { 'build',
        requires = {
            'freetype',
            'fribidi'
        }
    }
}
