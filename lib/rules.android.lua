package {
    name   = 'libuv',
    config = 'target',
    build  = {
        options = '--disable-static'
    },
    { 'build'   },
    { 'install' }
}
