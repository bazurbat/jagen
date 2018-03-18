return {
    source = {
        type = 'dist',
        location = 'https://dl.google.com/android/repository/android-ndk-r16b-linux-x86_64.zip',
        sha1sum = '42aa43aae89a50d1c66c3f9fdecd676936da6128',
        dir = 'android-ndk-r16b'
    },
    build = {
        toolchain = false
    },
    install = true
}

