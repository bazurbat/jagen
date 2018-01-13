return {
    source = {
        type = 'dist',
        location = 'arm-hisiv200-linux.tar.bz2',
        sha256sum = 'aa10eeed8ec7c7ff794683ff9c3e4c36fb327798db7d4616355bf02d43da24ba',
    },
    build = {
        arch   = 'arm',
        system = 'arm-hisiv200-linux-gnueabi',
        cpu    = 'cortex-a9',
        cflags = '-march=armv7-a -mcpu=cortex-a9 -mfpu=vfpv3-d16 -mfloat-abi=softfp',
        in_source = true,
        toolchain = false
    },
    install = {
        type = 'toolchain'
    }
}
