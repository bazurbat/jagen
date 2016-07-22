return {
    source = {
        type      = 'dist',
        location  = 'https://www.kernel.org/pub/linux/network/connman/connman-1.32.tar.xz',
        sha256sum = '3185864c73206a6033d12e9f583689dcd03f714a40a58333709d3f74a4e0934c'
    },
    patches = {
        { 'connman-1.32-add-missing-gnu-source', 1 }
    },
    build  = {
        type = 'GNU',
        options = {
            '--sysconfdir=/etc',
            '--localstatedir=/settings',
            -- connmand SEGVs on start without pie
            '--enable-pie',
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
    install = {
        dbus_system_configs = {
            "src/connman-dbus.conf"
        },
    },
    requires = {
        'dbus',
        'glib',
        'xtables-addons',
    }
}
