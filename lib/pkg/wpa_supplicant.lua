return {
    source = {
        type      = 'dist',
        location  = 'https://w1.fi/releases/wpa_supplicant-2.2.tar.gz',
        sha256sum = 'e0d8b8fd68a659636eaba246bb2caacbf53d22d53b2b6b90eb4b4fef0993c8ed'
    },
    patches = {
        { 'wpa_supplicant-2.2-do-not-call-dbus-functions-with-NULL-path', 1 }
    },
    build = {
        type = 'make',
        dir  = '$pkg_source_dir/wpa_supplicant',
        set_toolchain = true
    },
    requires = { 'dbus' }
}
