return {
    source = {
        location  = 'https://ftp.gnu.org/gnu/gdb/gdb-7.12.1.tar.xz',
        sha256sum = '4607680b973d3ec92c30ad029f1b7dbde3876869e6b3a117d8a7e90081113186'
    },
    build  = {
        type = 'gnu',
        options = {
            -- specify both build and host explicitly here so a user can set
            -- their desired target in a project
            '--build=$(jagen_get_system)',
            '--host=$(jagen_get_system)',
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
