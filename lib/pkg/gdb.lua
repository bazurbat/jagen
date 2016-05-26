return {
    source = 'gdb-7.9.tar.xz',
    build  = {
        type = 'GNU',
        options = {
            '--target=$jagen_target_system',
            '--program-transform-name=',
            '--disable-binutils',
            '--disable-etc',
            '--disable-gas',
            '--disable-gold',
            '--disable-gprof',
            '--disable-gdbserver',
            '--disable-readline',
            '--with-system-readline',
            '--with-zlib'
        }
    }
}
