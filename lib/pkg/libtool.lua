return {
    source = {
        type      = 'dist',
        location  = 'https://ftp.gnu.org/gnu/libtool/libtool-2.4.3.tar.xz',
        sha256sum = '65f940fbcb5f0727b10d759898afa35307b24c0945fb12f3f8387f5adb862ceb'
    },
    patches = {
        { 'libtool-2.4.3-no-clean-gnulib',   1 },
        { 'libtool-2.4.3-test-cmdline_wrap', 1 }
    },
    build = {
        type = 'gnu',
        options = {
            '--disable-ltdl-install'
        }
    }
}
