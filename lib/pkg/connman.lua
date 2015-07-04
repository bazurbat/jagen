package {
    name  = 'connman',
    build = {
        type = 'GNU',
        need_libtool = true
    },
    source = 'connman-1.28.tar.xz',
    { 'build',
        needs = { 'dbus', 'glib', 'xtables-addons' }
    }
}
