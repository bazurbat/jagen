return {
    source = {
        type      = 'dist',
        location  = 'https://releases.linaro.org/components/toolchain/binaries/4.9-2017.01/arm-linux-gnueabi/gcc-linaro-4.9.4-2017.01-x86_64_arm-linux-gnueabi.tar.xz',
        sha256sum = '232302236c90ec136a42d44bc387d2e1199f5df010b77e97ee590216d2b5b181',
    },
    build = {
        arch   = 'arm',
        system = 'arm-linux-gnueabi',
        toolchain = false
    },
    install = 'toolchain'
}
