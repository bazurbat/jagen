return {
    source = {
        location  = 'https://dl.google.com/android/repository/sdk-tools-linux-4333796.zip',
        basename  = 'tools',
        sha256sum = '92ffee5a1d98d856634e8b71132e8a95d96c83a63fde1099be3d86df3106def9',
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
