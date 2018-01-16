return {
    source = {
        type = 'dist',
        location = 'https://w1.fi/releases/hostapd-2.5.tar.gz',
        sha256sum = '8e272d954dc0d7026c264b79b15389ec2b2c555b32970de39f506b9f463ec74a'
    },
    build = {
        type = 'make',
        dir  = '$pkg_source_dir/hostapd',
        in_source = true,
        set_toolchain = true
    },
    requires = {
        'libnl'
    }
}
