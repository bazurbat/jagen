package {
    source = 'pango-1.36.8.tar.xz',
    build = {
        type = 'GNU',
        autoreconf = true,
        options = {
            '--disable-debug',
            '--disable-introspection',
            '--disable-gtk-doc',
            '--disable-gtk-doc-html',
            '--disable-gtk-doc-pdf',
            '--disable-explicit-deps',
            '--without-xft',
            '--with-cairo',
        }
    },
    requires = { 'cairo', 'fontconfig', 'harfbuzz' }
}
