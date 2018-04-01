return {
    source = {
        type      = 'dist',
        location  = 'https://releases.linaro.org/components/toolchain/binaries/6.4-2017.11/arm-linux-gnueabi/gcc-linaro-6.4.1-2017.11-x86_64_arm-linux-gnueabi.tar.xz',
        sha256sum = 'a7af327e8a07b709eb794ae5ecc21940dee1a60779f0756efc9f8e1ec992f5f6',
    },
    build = {
        arch   = 'arm',
        system = 'arm-linux-gnueabi',
        toolchain = false
    },
    install = 'toolchain'
}
