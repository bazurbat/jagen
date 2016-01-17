package {
    name   = 'android',
    source = {
        type     = 'repo',
        location = 'https://android.googlesource.com/platform/manifest',
        branch   = 'android-4.2.2_r1',
        path     = 'android'
    },
    build = {
        in_source = true
    }
}
