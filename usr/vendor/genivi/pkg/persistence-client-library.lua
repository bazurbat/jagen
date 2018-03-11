return {
    source = {
        type     = 'git',
        location = 'https://github.com/GENIVI/persistence-client-library.git',
        tag      = 'v1.1.0',
    },
    build = {
        type = 'gnu',
        autoreconf = true,
        in_source = true
    },
    requires = {
        'dbus',
        'persistence-common-object',
    }
}
