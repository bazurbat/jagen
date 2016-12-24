return {
    source = {
        type = 'dist',
        location = 'https://releases.linaro.org/archive/15.06/components/toolchain/binaries/4.8/arm-linux-gnueabi/runtime-linaro-gcc4.8-2015.06-arm-linux-gnueabi.tar.xz',
        sha256sum = '6a7a6b82280b3e30fd0425aa864a9614b52250333d3b3c789c3655b8c0496897',
    },
    build = {
        in_source = true,
        toolchain = false
    }
}
