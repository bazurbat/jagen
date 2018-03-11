return {
    source = {
        type     = 'git',
        location = 'https://github.com/otcshare/automotive-message-broker.git',
        branch   = '0.13',
    },
    build = {
        type = 'cmake',
        options = {
            '-Denable_icecc=OFF'
        }
    },
    requires = {
        'boost:system',
        'glib:system',
        'json-c:system',
        'uuid:system',
    }
}
