package {
    source = 'harfbuzz-0.9.41.tar.bz2',
    build = {
        type = 'GNU',
        autoreconf = true,
        options = {
            '--disable-silent-rules',
            '--disable-static',
            '--enable-shared',
            '--disable-gtk-doc',
            '--disable-gtk-doc-html',
            '--disable-gtk-doc-pdf',
            '--disable-introspection',
            '--with-glib',
            '--without-gobject',
            '--without-cairo',
            '--without-fontconfig',
            '--without-icu',
            '--without-graphite2',
            '--with-freetype',
            '--without-uniscribe',
            '--without-coretext'
        },
        libs = { 'harfbuzz' }
    },
    requires = { 'glib', 'freetype' }
}
