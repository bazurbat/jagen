return {
    source = {
        type      = 'dist',
        location  = 'http://dist.libuv.org/dist/v1.4.2/libuv-v1.4.2.tar.gz',
        sha256sum = '1006c4055cb3fb34293bb716be127d396550a68ac0bdfc21a6631c8d7a49ce0b'
    },
    build  = {
        type    = 'gnu',
        options = { '--disable-static' },
        libs    = { 'uv' },
        generate = true
    }
}
