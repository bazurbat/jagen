return {
    source = {
        type      = 'dist',
        location  = 'http://ftp.gnome.org/pub/GNOME/sources/pango/1.36/pango-1.36.8.tar.xz',
        sha256sum = '18dbb51b8ae12bae0ab7a958e7cf3317c9acfc8a1e1103ec2f147164a0fc2d07'
    },
    build = {
        type = 'gnu',
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
