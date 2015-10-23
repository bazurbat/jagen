package {
    name   = 'soundtouch',
    source = 'soundtouch-1.9.2.tar.gz',
    build  = {
        type = 'GNU',
        options = '--enable-integer-samples=yes --enable-x86-optimizations=no',
        libs = { 'SoundTouch' }
    }
}
