return {
    source = {
        type      = 'dist',
        location  = 'http://ftp.gnome.org/pub/GNOME/sources/glib/2.40/glib-2.40.2.tar.xz',
        sha256sum = 'e8ff8af2950897e805408480c454c415d1eade4e670ec5fb507f5e5853726c7a'
    },
    patches = {
        { 'glib-2.40.0-external-gdbus-codegen', 1 }
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
