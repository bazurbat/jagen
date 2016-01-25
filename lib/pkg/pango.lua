package {
    source = 'pango-1.36.8.tar.xz',
    build = {
        type    = 'GNU',
        options = {
            '--disable-debug',
            '--disable-introspection',
            '--disable-gtk-doc-html',
            '--disable-explicit-deps',
            '--without-xft',
            '--with-cairo',
        }
    },
    requires = { 'cairo' }
}
