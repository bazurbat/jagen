package {
    name    = 'wpa_supplicant',
    build   = {
        type = 'make'
    },
    source  = 'wpa_supplicant-2.2.tar.gz',
    patches = {
        { 'wpa_supplicant-2.2-do-not-call-dbus-functions-with-NULL-path', 1 }
    },
    { 'build',
        needs = { 'dbus' }
    }
}
