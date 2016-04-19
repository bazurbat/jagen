rule {
    source = {
        type = 'dist',
        location = 's6-portable-utils-2.0.5.3.tar.gz'
    },
    build = {
        type = 'skarnet'
    },
    requires = {
        'skalibs'
    }
}
