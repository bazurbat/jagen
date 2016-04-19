rule {
    source = {
        type = 'dist',
        location = 's6-2.2.4.3.tar.gz'
    },
    build = {
        type = 'skarnet'
    },
    requires = {
        'execline',
        'skalibs',
    }
}
