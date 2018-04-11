return { 'android-sdk-tools',
    source = {
        location = 'https://dl.google.com/android/repository/sdk-tools-linux-3859397.zip',
        basename = 'tools',
        sha1sum  = '7eab0ada7ff28487e1b340cc3d866e70bcb4286e',
    },
    install = true,
    export = {
        env = {
            -- ANROID_HOME is deprecated but Gradle plugin does not know this
            ANDROID_HOME = "$pkg_source_dir/..",
            ANDROID_SDK_ROOT = "$pkg_export_env_ANDROID_HOME",
        }
    }
}
