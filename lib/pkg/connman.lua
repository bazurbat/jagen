package {
    name   = 'connman',
    source = 'connman-1.28.tar.xz',
    build  = {
        type = 'GNU',
        autoreconf = true,
        options = {
            '--sysconfdir=/etc',
            '--localstatedir=/settings',
            '--enable-pie',
            '--disable-gadget',
            '--disable-bluetooth',
            '--disable-ofono',
            '--disable-dundee',
            '--disable-pacrunner',
            '--disable-neard',
            '--disable-wispr',
            '--disable-client'
        }
    },
    requires = { 'dbus', 'glib', 'xtables-addons' }
}
