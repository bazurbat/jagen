return {
    source = {
        location  = 'https://ftp.gnu.org/gnu/gdb/gdb-8.2.tar.xz',
        sha256sum = 'c3a441a29c7c89720b734e5a9c6289c0a06be7e0c76ef538f7bbcef389347c39'
    },
    build  = {
        type = 'gnu',
        options = {
            '--build=x86_64-linux-gnu',
            '--host=x86_64-linux-gnu',
            '--disable-binutils',
            '--disable-etc',
            '--disable-gas',
            '--disable-gold',
            '--disable-ld',
            '--disable-gprof',
            '--disable-gdbserver'
        }
    }
}
