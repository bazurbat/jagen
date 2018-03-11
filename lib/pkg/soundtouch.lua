return {
    source = {
        type      = 'dist',
        location  = 'http://www.surina.net/soundtouch/soundtouch-1.9.2.tar.gz',
        sha256sum = 'caeb86511e81420eeb454cb5db53f56d96b8451d37d89af6e55b12eb4da1c513',
        dir       = 'soundtouch'
    },
    build = {
        type = 'gnu',
        options = {
            '--enable-integer-samples=yes',
            '--enable-x86-optimizations=no'
        },
        autoreconf = true
    },
    install = {
        libs = { 'SoundTouch' }
    }
}
