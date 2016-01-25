package {
    name   = 'fontconfig',
    source = 'fontconfig-2.11.1.tar.bz2',
    build  = {
        type = 'GNU',
        options = {
            '--disable-docs'
        },
        libs = { 'fontconfig' }
    },
    requires = { 'expat', 'freetype' }
}
