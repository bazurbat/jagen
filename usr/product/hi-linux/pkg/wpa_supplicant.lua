return {
    source = 'wpa_supplicant-2.2.tar.gz',
    patches = {
        { 'wpa_supplicant-2.2-do-not-call-dbus-functions-with-NULL-path', 1 }
    },
    build = {
        type = 'make',
        dir  = '$pkg_source_dir/wpa_supplicant'
    },
    requires = { 'dbus', 'libnl' }
}
