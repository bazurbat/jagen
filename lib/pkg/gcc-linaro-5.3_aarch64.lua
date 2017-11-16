return {
    source = {
        type = 'dist',
        location = 'http://releases.linaro.org/components/toolchain/binaries/5.3-2016.05/aarch64-linux-gnu/gcc-linaro-5.3.1-2016.05-x86_64_aarch64-linux-gnu.tar.xz',
        sha256sum = '1941dcf6229d6706bcb89b7976d5d43d170efdd17c27d5fe1738e7ecf22adc37',
    },
    build = {
        in_source = true,
        requires_toolchain = false
    },
    requires = {
        'gcc-linaro-5.3_aarch64-runtime',
        'gcc-linaro-5.3_aarch64-sysroot'
    }
}
