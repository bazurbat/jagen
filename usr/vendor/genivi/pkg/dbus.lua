return {
    source = {
        type      = 'dist',
        location  = 'https://dbus.freedesktop.org/releases/dbus/dbus-1.10.14.tar.gz',
        sha256sum = '23238f70353e38ce5ca183ebc9525c0d97ac00ef640ad29cf794782af6e6a083'
    },
    patches = {
        provider = 'capicxx-dbus-runtime',
        dir = 'src/dbus-patches',
        { 'capi-dbus-1-pc',                                            1 },
        { 'capi-dbus-add-send-with-reply-set-notify',                  1 },
        { 'capi-dbus-add-support-for-custom-marshalling',              1 },
        { 'capi-dbus-block-acquire-io-path-on-send',                   1 },
        { 'capi-dbus-correct-dbus-connection-block-pending-call',      1 },
        { 'capi-dbus-send-with-reply-and-block-delete-reply-on-error', 1 },
    },
    build  = {
        type = 'gnu',
        -- fails to find lexpat on target otherwise
        configure_needs_install_dir = true,
        options = {
            '--enable-shared',
            '--disable-static',
            '--disable-compiler-coverage',
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
            '--without-x',
        }
    },
    install = {
        libs = { 'dbus-1' }
    },
    requires = { 'expat' },
}
