return {
    source = {
        type      = 'dist',
        location  = 'ftp://sourceware.org/pub/libffi/libffi-3.1.tar.gz',
        sha256sum = '97feeeadca5e21870fa4433bc953d1b3af3f698d5df8a428f68b73cd60aef6eb'
    },
    patches = {
        { 'libffi-3.1-execstack', 0 },
        { 'libffi-3.1-typing_error', 0 }
    },
    build = {
        type    = 'GNU',
        options = { '--disable-builddir' },
        libs    = { 'ffi' },
        autoreconf = true
    }
}
