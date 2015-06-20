package {
    name    = 'libffi',
    source  = 'libffi-3.1.tar.gz',
    patches = {
        { 'libffi-3.1-execstack', 0 },
        { 'libffi-3.1-typing_error', 0 }
    }
}
