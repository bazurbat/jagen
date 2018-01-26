return {
    source = {
        type      = 'dist',
        location  = 'http://zlib.net/zlib-1.2.11.tar.gz',
        sha256sum = 'c3e5e9fdd5004dcb542feda5ee4f0ff0744628baf8ed2dd5d66f8ca1197cb1a1'
    },
    build = {
        type = 'make',
        -- upstream default, should be safe
        cflags = '-O3'
    }
}
