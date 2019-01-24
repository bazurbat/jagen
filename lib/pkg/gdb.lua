return {
    source = {
        type      = 'dist',
        location  = 'https://ftp.gnu.org/gnu/gdb/gdb-7.12.1.tar.xz',
        sha256sum = '4607680b973d3ec92c30ad029f1b7dbde3876869e6b3a117d8a7e90081113186'
    },
    build  = {
        type = 'gnu',
        options = {
            '--build=x86_64-linux-gnu',
            '--host=x86_64-linux-gnu',
            '--target=${target_toolchain_system}',
            '--disable-binutils',
            '--disable-etc',
            '--disable-gas',
            '--disable-gold',
            '--disable-gprof',
            '--disable-gdbserver'
        }
    },
}
