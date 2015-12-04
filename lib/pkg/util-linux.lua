package {
    name   = 'util-linux',
    source = 'util-linux-2.23.2.tar.xz',
    patches = {
        { 'util-linux-2.23.2', 1 }
    },
    build = {
        type = 'GNU',
        with_provided_libtool = true
    }
}
