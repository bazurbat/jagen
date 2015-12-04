package {
    name   = 'libuv',
    source = 'libuv-1.4.2.tar.gz',
    build  = {
        type    = 'GNU',
        options = '--disable-static',
        libs    = { 'uv' }
    }
}
