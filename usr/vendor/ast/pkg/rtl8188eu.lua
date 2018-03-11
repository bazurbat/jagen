return {
    source = {
        type      = 'dist',
        location  = 'file:///$jagen_src_dir/hi-sdk/source/component/wifi/drv/usb_rtl8188eu/rtl8188EUS_linux_v4.3.0.6_12167.20140828.tar.gz',
        sha256sum = '037d1e1ec90fd86bfa37af23665758a72708afe5fb7955848d45ffadfc30bbcf'
    },
    build = {
        type = 'linux-module',
        options = {
            'KSRC=$KERNEL_SRC'
        },
        in_source = true
    },
    use = 'kernel',
    -- the source tarball is distributed with hi-sdk
    { 'unpack',
        { 'hi-sdk', 'unpack' }
    }
}
