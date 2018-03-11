return {
    source = {
        type     = 'git',
        location = 'https://github.com/GENIVI/persistence-administrator.git',
        tag      = '1.0.5'
    },
    patches = {
        { 'persistence-administrator-1.0.5-fix-libsystemd-pc-filename', 1 },
    },
    build = {
        type = 'gnu',
        autoreconf = true,
        in_source = true,
        options = {
            -- _ISOC99_SOURCE is an internal flag for persistence-common-object
            -- which disables its definitions of std int types
            -- _DEFAULT_SOURCE is needed for lockf function which
            -- persistence-administrator uses but does not checks
            'CFLAGS=-D_ISOC99_SOURCE -D_DEFAULT_SOURCE -g',
            '--with-systemdsystemunitdir=$pkg_install_dir/lib/systemd/system'
        }
    },
    requires = {
        'dbus', -- >= 1.4.10
        'dlt-daemon', -- >= 2.2.0
        'glib', -- >= 2.30.0
        'node-state-manager',
        'persistence-common-object', -- >= 1.0.1
        'json:system', -- >= 0.9
        'libarchive:system', -- >= 3.0.4
        'systemd:system', -- >= 37
        'zlib:system', -- >= 1.2.5
    }
}
