return {
    source = {
        type = 'git',
        location = 'https://github.com/GENIVI/persistence-common-object.git',
        branch = '1.0.3',
    },
    build = {
        type = 'GNU',
        autoreconf = true,
        in_source = true
    }
}