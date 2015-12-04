package {
    name   = 'soundtouch',
    source = {
        type      = 'dist',
        location  = 'soundtouch-1.9.2.tar.gz',
        directory = 'soundtouch'
    },
    build = {
        type = 'GNU',
        options = '--enable-integer-samples=yes --enable-x86-optimizations=no',
        libs = { 'SoundTouch' }
    }
}
