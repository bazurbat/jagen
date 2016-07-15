return {
    source = {
        type      = 'dist',
        location  = 'https://github.com/systemd/systemd/archive/v216.tar.gz',
        filename  = 'systemd-216.tar.gz',
        sha256sum = '',
        dir       = 'systemd-216'
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
