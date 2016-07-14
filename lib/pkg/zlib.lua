return {
    source = {
        type     = 'dist',
        location = 'http://zlib.net/zlib-1.2.8.tar.gz',
        md5sum   = '44d667c142d7cda120332623eab69f40'
    },
    build  = {
        type = 'make',
        in_source = true
    }
}
