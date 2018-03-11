return {
    source = {
        type     = 'git',
        location = 'https://github.com/systemd/systemd.git',
        tag      = 'v208'
    },
    build = {
        type = 'gnu',
        generate = true,
        options = {
        },
    },
    requires = {
        'libcap'
    }
}
