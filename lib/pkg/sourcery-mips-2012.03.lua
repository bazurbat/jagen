return {
    source = {
        type      = 'dist',
        -- https://sourcery.mentor.com/GNUToolchain/subscription3130?lite=MIPS
        location  = 'https://sourcery.mentor.com/GNUToolchain/package10395/public/mips-linux-gnu/mips-2012.03-63-mips-linux-gnu-i686-pc-linux-gnu.tar.bz2',
        sha256sum = '0a2d92bbca2926479662affcc4dcb644ddca7d8d50ec17ed3b99d7025a552530',
        md5sum    = '44970134eea6f6ab754a7bbe6e2805d6',
        dir       = 'mips-2012.03'
    },
    build = {
        arch   = 'mips',
        system = 'mipsel-linux-gnu',
        cpu    = '24kf',
        cflags = '-march=24kf -mtune=24kf -Wa,-mips32r2',
        in_source = true,
        toolchain = false
    },
    install = {
        type = 'toolchain'
    }
}
