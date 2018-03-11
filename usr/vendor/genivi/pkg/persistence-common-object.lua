return {
    source = {
        type     = 'git',
        location = 'https://github.com/GENIVI/persistence-common-object.git',
        tag      = '1.0.3',
    },
    build = {
        type = 'gnu',
        autoreconf = true,
        in_source = true,
        options = {
            '--with-database=key-value-store'
        }
    }
}
