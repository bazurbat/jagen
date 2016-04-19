rule {
    source = {
        type = 'dist',
        location = 'runit-2.1.2.tar.gz',
        dir = 'admin/runit-2.1.2'
    },
    build = {
        type = 'make',
        in_source = true
    }
}
