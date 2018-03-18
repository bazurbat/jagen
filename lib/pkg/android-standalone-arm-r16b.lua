return {
    build = {
        type      = 'android-standalone-toolchain',
        toolchain = 'android-ndk-r16b',
        arch      = 'arm',
        system    = 'arm-linux-androideabi'
    },
    export = {
        cmake_options = {
            '-DCMAKE_TOOLCHAIN_FILE=${toolchain_source_dir}/build/cmake/android.toolchain.cmake',
            '-DANDROID_ABI=armeabi-v7a',
            '-DANDROID_PLATFORM=android-23'
        }
    }
}
