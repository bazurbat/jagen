return {
    source = {
        type     = 'dist',
        location = 'https://dl.google.com/android/repository/android-ndk-r17b-linux-x86_64.zip',
        sha1sum  = 'dd5762ee7ef4995ad04fe0c45a608c344d99ca9f',
        basename = 'android-ndk-r17b'
    },
    export = {
        env = {
            ANDROID_NDK_HOME = "$pkg_source_dir"
        }
    },
    { 'install' }
}
