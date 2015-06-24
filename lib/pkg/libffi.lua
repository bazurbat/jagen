package {
    name  = 'libffi',
    build = {
        type = 'GNU',
        libs = { 'ffi' },
        need_libtool = true
    },
    source  = 'libffi-3.1.tar.gz',
    patches = {
        { 'libffi-3.1-execstack', 0 },
        { 'libffi-3.1-typing_error', 0 }
    }
}
