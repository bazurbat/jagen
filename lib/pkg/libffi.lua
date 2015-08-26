package {
    name  = 'libffi',
    build = {
        type    = 'GNU',
        options = '--disable-builddir',
        libs    = { 'ffi' },
        with_provided_libtool = true
    },
    source  = 'libffi-3.1.tar.gz',
    patches = {
        { 'libffi-3.1-execstack', 0 },
        { 'libffi-3.1-typing_error', 0 }
    }
}
