return {
    source = {
        type = 'dist',
        location = 'https://releases.linaro.org/15.06/components/toolchain/binaries/4.8/arm-linux-gnueabi/sysroot-linaro-glibc-gcc4.8-2015.06-arm-linux-gnueabi.tar.xz',
        sha256sum = '45f9c9b31faa235b08d3339ba2598644ebcd753496893f4b24b24797f46b3d0b',
    },
    build = {
        in_source = true,
        toolchain = false
    }
}
