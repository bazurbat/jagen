return {
    source = {
        type      = 'dist',
        location  = 'https://dbus.freedesktop.org/releases/dbus/dbus-1.10.14.tar.gz',
        sha256sum = '23238f70353e38ce5ca183ebc9525c0d97ac00ef640ad29cf794782af6e6a083'
    },
    build  = {
        type = 'GNU',
        -- fails to find lexpat otherwise
        -- configure_needs_install_dir = true,
        libs = { 'dbus-1' },
        options = {
            '--with-system-pid-file=/run/dbus.pid',
            '--with-system-socket=/run/dbus/system_bus_socket',
            '--enable-shared',
            '--disable-static',
            '--disable-compiler-coverage',
            '--enable-compiler-optimizations',
            '--disable-developer',
            '--disable-ansi',
            '--disable-verbose-mode',
            '--disable-asserts',
            '--enable-checks',
            '--disable-xml-docs',
            '--disable-doxygen-docs',
            '--disable-ducktype-docs',
            '--enable-abstract-sockets',
            '--disable-selinux',
            '--disable-apparmor',
            '--disable-libaudit',
            '--enable-inotify',
            '--disable-kqueue',
            '--disable-console-owner-file',
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
    requires = { 'expat' },
    -- patches are distributed with the capicxx-dbus-runtime package
    { 'patch',
        { 'capicxx-dbus-runtime', 'unpack' }
    }
}
