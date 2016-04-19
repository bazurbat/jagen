rule {
    source = 'pango-1.36.8.tar.xz',
    build = {
        type = 'GNU',
        options = {
            '--disable-static',
            '--enable-shared',
            '--disable-debug',
            '--disable-introspection',
            '--disable-gtk-doc',
            '--disable-gtk-doc-html',
            '--disable-gtk-doc-pdf',
            '--disable-man',
            '--disable-doc-cross-references',
            '--disable-explicit-deps',
            '--without-xft',
            '--with-cairo',
        }
    },
    requires = {
        'cairo',
        'fontconfig',
        'harfbuzz'
    }
}
