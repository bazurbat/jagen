rule {
    source = 'dbus-1.6.18.tar.gz',
    build  = {
        type = 'GNU',
        -- fails to find lexpat otherwise
        configure_needs_install_dir = true,
        libs = { 'dbus-1' },
        options = {
            '--with-system-pid-file=/run/dbus.pid',
            '--with-system-socket=/run/dbus/system_bus_socket',
            '--disable-compiler-coverage',
            '--disable-developer',
            '--disable-ansi',
            '--disable-verbose-mode',
            '--disable-asserts',
            '--disable-checks',
            '--disable-xml-docs',
            '--disable-doxygen-docs',
            '--disable-abstract-sockets',
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
            '--disable-epoll',
            '--disable-x11-autolaunch',
            '--disable-Werror',
            '--disable-stats',
            '--with-xml=expat',
            '--without-valgrind',
            '--without-x'
        }
    },
    requires = { 'expat' }
}
