return {
    source = {
        type      = 'git',
        location  = 'https://github.com/systemd/systemd.git',
        branch    = 'v208'
    },
    build = {
        type = 'GNU',
        generate = true,
        options = {
        },
    },
    requires = {
        'libcap'
    }
}
