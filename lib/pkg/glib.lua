package {
    source = 'glib-2.40.2.tar.xz',
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
