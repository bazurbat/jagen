return {
    source = {
        type = 'dist',
        location = 'https://releases.linaro.org/archive/15.06/components/toolchain/binaries/4.8/arm-linux-gnueabi/gcc-linaro-4.8-2015.06-x86_64_arm-linux-gnueabi.tar.xz',
        sha256sum = '04556b1a453008222ab55e4ab9d792f32b6f8566e46e99384225a6d613851587',
    },
    build = {
        arch   = 'arm',
        system = 'arm-linux-gnueabi',
        cflags = '-march=armv7-a -mcpu=cortex-a9 -mfpu=vfpv3-d16 -mfloat-abi=softfp',
        in_source = true,
        toolchain = false
    },
    install = {
        type = 'toolchain'
    }
}
