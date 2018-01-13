return {
    source = {
        type = 'dist',
        location = 'https://releases.linaro.org/archive/15.06/components/toolchain/binaries/4.8/arm-linux-gnueabi/gcc-linaro-4.8-2015.06-x86_64_arm-linux-gnueabi.tar.xz',
        sha256sum = '04556b1a453008222ab55e4ab9d792f32b6f8566e46e99384225a6d613851587',
    },
    build = {
        target_arch = 'arm',
        target_system = '${pkg_build_target_arch}-linux-gnueabi',
        in_source = true,
        toolchain = false
    }
}
