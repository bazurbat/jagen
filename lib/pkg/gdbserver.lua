return {
    source = {
        type      = 'dist',
        location  = 'http://ftp.gnu.org/gnu/gdb/gdb-7.9.tar.xz',
        sha256sum = '9b315651a16528f7af8c7d8284699fb0c965df316cc7339bb0b7bae335848392',
    },
    build = {
        type = 'gnu',
        in_source = true,
        options = {
            '--disable-werror',
            '--program-transform-name=',
        },
        dir = '$pkg_source_dir/gdb/gdbserver',
        configure_file = "./configure"
    }
}
