return {
    source = {
        type = 'dist',
        location = 'https://busybox.net/downloads/busybox-1.24.2.tar.bz2',
        sha1sum = '03e6cfc8ddb2f709f308719a9b9f4818bc0a28d0'
    },
    build  = {
        type = 'kbuild',
        in_source = true
    }
}
