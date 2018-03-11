return {
    build = {
        arch   = 'arm',
        system = 'arm-linux-androideabi',
        toolchain = 'android-ndk'
    },
    install = {
        type = 'android-ndk-toolchain'
    },
    export = {
        cmake_options = {
            '-DCMAKE_TOOLCHAIN_FILE=${toolchain_source_dir}/build/cmake/android.toolchain.cmake',
            '-DANDROID_ABI=armeabi-v7a',
            '-DANDROID_PLATFORM=android-23'
        }
    }
}
