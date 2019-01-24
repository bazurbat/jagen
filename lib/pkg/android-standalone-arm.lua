return {
    build = {
        type      = 'android-standalone-toolchain',
        toolchain = 'android-ndk',
        system    = 'arm-linux-androideabi',
        cc        = 'clang',
        cxx       = 'clang++',
    },
    export = {
        -- -- Android 5.0 (API >= 21) only supports PIE
        ldflags = '-pie -fPIE',
        cmake_options = {
            '-DCMAKE_POSITION_INDEPENDENT_CODE=YES',
            -- CMake scripts use this to switch behaviour
            '-DANDROID=YES',
        }
    }
}
