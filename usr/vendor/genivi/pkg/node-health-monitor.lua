return {
    source = {
        type     = 'git',
        location = 'https://github.com/GENIVI/node-health-monitor.git',
        tag      = '1.3.5',
    },
    build = {
        type = 'gnu',
        autoreconf = true,
    },
    requires = {
        'dbus',
        'dlt-daemon',
        'glib',
        'node-state-manager',
        'persistence-client-library',
        'systemd:system',
    }
}
