return {
    source = {
        type     = 'dist',
        location = 'https://dl.google.com/android/repository/android-ndk-r16b-linux-x86_64.zip',
        sha1sum  = '42aa43aae89a50d1c66c3f9fdecd676936da6128',
        basename = 'android-ndk-r16b'
    },
    export = {
        env = {
            ANDROID_NDK_HOME = "$pkg_source_dir"
        }
    },
    { 'install' }
}
