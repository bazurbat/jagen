return {
    source = {
        type      = 'dist',
        location  = 'https://releases.linaro.org/components/toolchain/binaries/5.4-2017.05/arm-linux-gnueabi/gcc-linaro-5.4.1-2017.05-x86_64_arm-linux-gnueabi.tar.xz',
        sha256sum = 'ac60d6831d4053b94e02865dbab6b42ca2be3eda97f82bfa7d6747f4546dcb1f',
    },
    build = {
        system = 'arm-linux-gnueabi',
        toolchain = false
    },
    install = 'toolchain'
}
