return {
    source = {
        type      = 'dist',
        location  = 'http://ftp.gnome.org/pub/GNOME/sources/glib/2.48/glib-2.48.2.tar.xz',
        sha256sum = 'f25e751589cb1a58826eac24fbd4186cda4518af772806b666a3f91f66e6d3f4'
    },
    build = {
        type = 'GNU',
        options = {
            '--disable-mem-pools',
            '--disable-rebuilds',
            '--disable-selinux',
            '--disable-fam',
            '--disable-xattr',
            '--disable-libelf',
            '--disable-gtk-doc-html',
            '--disable-man',
            '--disable-dtrace',
            '--disable-systemtap',
            '--disable-coverage',
            '--disable-Bsymbolic',
            '--disable-znodelete'
        },
        libs = { 'glib-2.0', 'gthread-2.0', 'gobject-2.0', 'gmodule-2.0', 'gio-2.0' },
        autoreconf = true
    },
    requires = { 'libffi', 'zlib' }
}
