package {
    source = 'fontconfig-2.11.1.tar.bz2',
    build  = {
        type = 'GNU',
        workaround_libtool_rpath = true,
        options = {
            '--disable-static',
            '--enable-shared',
            '--enable-iconv',
            '--disable-libxml2',
            '--disable-docs',
        },
        libs = { 'fontconfig' }
    },
    requires = { 'expat', 'freetype' }
}
