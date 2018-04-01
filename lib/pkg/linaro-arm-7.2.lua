return {
    source = {
        type = 'dist',
        location = 'https://releases.linaro.org/components/toolchain/binaries/7.2-2017.11/arm-linux-gnueabi/gcc-linaro-7.2.1-2017.11-x86_64_arm-linux-gnueabi.tar.xz',
        sha256sum = '89e9bfc7ffe615f40a72c2492df0488f25fc20404e5f474501c8d55941337f71',
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
