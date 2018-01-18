return {
    source = {
        type = 'dist',
        location = 'mips-2012.03-63-mips-linux-gnu-i686-pc-linux-gnu.tar.bz2',
        sha256sum = '0a2d92bbca2926479662affcc4dcb644ddca7d8d50ec17ed3b99d7025a552530',
        dir = 'mips-2012.03'
    },
    build = {
        arch   = 'mips',
        system = 'mipsel-linux-gnu',
        cpu    = '24kf',
        cflags = '-march=24kf -mtune=24kf -Wa,-mips32r2',
        in_source = true,
        toolchain = false
    }
}
