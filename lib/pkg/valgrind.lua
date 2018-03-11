return {
    source = 'valgrind-3.10.1.tar.bz2',
    patches = {
        { 'valgrind-3.10.1-linux-4',            1 },
        { 'valgrind-3.10.1-non-exec-stack',     1 },
        { 'valgrind-3.6.0-local-labels',        0 },
        { 'valgrind-3.7.0-fno-stack-protector', 1 },
        { 'valgrind-3.7.0-respect-flags',       1 },
        { 'valgrind-3.9.0-glibc-2.19',          1 },
    },
    build = {
        type    = 'gnu',
        options = {
            '--enable-only32bit',
            '--disable-tls',
            '--with-pagesize=16',
            '--without-mpicc'
        },
        autoreconf = true,
    }
}
