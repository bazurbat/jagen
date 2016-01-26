package {
    name   = 'libtool',
    source = 'libtool-2.4.3.tar.xz',
    patches = {
        { 'libtool-2.4.3-no-clean-gnulib',   1 },
        { 'libtool-2.4.3-test-cmdline_wrap', 1 }
    },
    build = {
        type = 'GNU',
        options = {
            '--disable-ltdl-install'
        }
    },
    install = {
        prefix = '$jagen_host_dir$jagen_host_prefix',
        path = ''
    }
}
