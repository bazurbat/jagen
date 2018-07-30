return {
    build = {
        type      = 'android-standalone-toolchain',
        toolchain = 'android-ndk',
        arch      = 'arm',
        system    = 'arm-linux-androideabi',
        cc        = 'clang',
        cxx       = 'clang++',
    },
    export = {
        cmake_options = {
            '-DANDROID=YES',
        }
    }
}
