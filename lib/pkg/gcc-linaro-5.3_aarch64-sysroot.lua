return {
    source = {
        type = 'dist',
        location = 'http://releases.linaro.org/components/toolchain/binaries/5.3-2016.05/aarch64-linux-gnu/sysroot-glibc-linaro-2.21-2016.05-aarch64-linux-gnu.tar.xz',
        sha256sum = '9b9228ff88e563e70e0988b3dcb641b915a191f7bbf5a3c949147aa12d6ed5ea',
    },
    build = {
        in_source = true,
        requires_toolchain = false
    }
}
