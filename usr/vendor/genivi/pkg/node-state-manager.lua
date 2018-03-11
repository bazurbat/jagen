return {
    source = {
        type     = 'git',
        location = 'https://github.com/GENIVI/node-state-manager.git',
        tag      = '2.0.0',
    },
    patches = {
        { 'node-state-manager-2.0.0-fix-libsystemd-pc-filename', 1 },
        { 'node-state-manager-2.0.0-mkdir-before-gdbus-codegen', 1 },
    },
    build = {
        type = 'gnu',
        autoreconf = true,
        in_source = true,
        options = {
            '--with-systemdsystemunitdir=$pkg_install_dir/lib/systemd/system'
        }
    },
    requires = {
        'dbus', -- >= 1.4.10
        'dlt-daemon', -- >= 2.2.0
        'glib', -- >= 2.30.0
        'persistence-client-library', -- >= 0.6.0
        'systemd:system', -- >= 37
    }
}
