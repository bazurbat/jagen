return {
    source = 'connman-1.32.tar.xz',
    patches = {
        { 'connman-1.32-add-missing-gnu-source', 1 }
    },
    build  = {
        type = 'GNU',
        options = {
            '--sysconfdir=/etc',
            '--localstatedir=/settings',
            '--disable-gadget',
            '--disable-bluetooth',
            '--disable-ofono',
            '--disable-dundee',
            '--disable-pacrunner',
            '--disable-neard',
            '--disable-wispr',
            '--disable-tools',
            '--disable-client',
            '--disable-datafiles',
        }
    },
    requires = {
        'dbus',
        'glib',
        'xtables-addons',
    }
}
