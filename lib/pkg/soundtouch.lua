rule {
    source = {
        type     = 'dist',
        location = 'soundtouch-1.9.2.tar.gz',
        dir      = 'soundtouch'
    },
    build = {
        type = 'GNU',
        options = {
            '--enable-integer-samples=yes',
            '--enable-x86-optimizations=no'
        },
        libs = { 'SoundTouch' },
        autoreconf = true
    }
}
