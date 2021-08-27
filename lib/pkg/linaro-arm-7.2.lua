package { 'linaro-arm-7.2',
    source = {
        type      = 'dist',
        location  = 'https://releases.linaro.org/components/toolchain/binaries/7.2-2017.11/arm-linux-gnueabi/gcc-linaro-7.2.1-2017.11-x86_64_arm-linux-gnueabi.tar.xz',
        sha256sum = '89e9bfc7ffe615f40a72c2492df0488f25fc20404e5f474501c8d55941337f71',
    },
    build = {
        system = 'arm-linux-gnueabi'
    },
    install = 'toolchain',
    export = {
        system = '${build.system}'
    }
}
