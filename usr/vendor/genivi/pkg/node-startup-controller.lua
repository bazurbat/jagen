return {
    source = {
        type     = 'git',
        location = 'https://github.com/GENIVI/node-startup-controller.git',
        tag      = 'node-startup-controller-1.0.2',
    },
    build = {
        type = 'gnu',
        autoreconf = true,
    },
    requires = {
        'dlt-daemon', -- >= 2.2.0
        'glib', -- >= 2.30.0
        'systemd:system', -- >= 183
    }
}
