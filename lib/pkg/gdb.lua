return {
    source = {
        type      = 'dist',
        location  = 'http://ftp.gnu.org/gnu/gdb/gdb-7.9.tar.xz',
        sha256sum = '9b315651a16528f7af8c7d8284699fb0c965df316cc7339bb0b7bae335848392'
    },
    build  = {
        type = 'gnu',
        options = {
            '--build=x86_64-linux-gnu',
            '--host=x86_64-linux-gnu',
            '--target=${target_toolchain_system}',
            '--program-transform-name=',
            '--disable-binutils',
            '--disable-etc',
            '--disable-gas',
            '--disable-gold',
            '--disable-gprof',
            '--disable-gdbserver',
            '--disable-readline',
            '--with-system-readline',
            '--with-system-zlib',
        }
    }
}
