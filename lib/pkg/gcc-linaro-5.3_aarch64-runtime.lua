return {
    source = {
        type = 'dist',
        location = 'http://releases.linaro.org/components/toolchain/binaries/5.3-2016.05/aarch64-linux-gnu/runtime-gcc-linaro-5.3.1-2016.05-aarch64-linux-gnu.tar.xz',
        sha256sum = '44f3237aeeac9199527d5169ac3632fcbdb6792cd57eb58b7e7ed1857f0aed37',
    },
    build = {
        in_source = true,
        requires_toolchain = false
    }
}
