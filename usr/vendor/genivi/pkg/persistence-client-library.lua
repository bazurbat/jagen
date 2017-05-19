return {
    source = {
        type = 'git',
        location = 'https://github.com/GENIVI/persistence-client-library.git',
        branch = 'v1.1.0',
    },
    build = {
        type = 'GNU',
        autoreconf = true,
        in_source = true
    },
    requires = {
        'dbus',
        'persistence-common-object',
    }
}
