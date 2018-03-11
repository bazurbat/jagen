return {
    source = {
        type      = 'dist',
        location  = 'https://dbus.freedesktop.org/releases/dbus/dbus-1.6.18.tar.gz',
        sha256sum = '7085a0895a9eb11a952394cdbea6d8b4358e17cb991fed0e8fb85e2b9e686dcd'
    },
    build  = {
        type = 'gnu',
        -- fails to find lexpat on target otherwise
        configure_needs_install_dir = true,
        options = {
            '--with-system-pid-file=/run/dbus.pid',
            '--with-system-socket=/run/dbus/system_bus_socket',
            '--enable-shared',
            '--disable-static',
            '--disable-compiler-coverage',
            '--disable-developer',
            '--disable-ansi',
            '--disable-verbose-mode',
            '--disable-asserts',
            '--enable-checks',
            '--disable-xml-docs',
            '--disable-doxygen-docs',
            '--enable-abstract-sockets',
            '--disable-selinux',
            '--disable-libaudit',
            '--disable-dnotify',
            '--enable-inotify',
            '--disable-kqueue',
            '--disable-console-owner-file',
            '--disable-userdb-cache',
            '--disable-launchd',
            '--disable-systemd',
            '--disable-embedded-tests',
            '--disable-modular-tests',
            '--disable-tests',
            '--disable-installed-tests',
            '--enable-epoll',
            '--disable-x11-autolaunch',
            '--disable-Werror',
            '--disable-stats',
            '--with-xml=expat',
            '--without-valgrind',
            '--without-x',
        }
    },
    install = {
        libs = { 'dbus-1' }
    },
    requires = { 'expat' }
}
