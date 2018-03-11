return {
    source = {
        type      = 'dist',
        location  = 'ftp://sourceware.org/pub/libffi/libffi-3.2.1.tar.gz',
        md5sum    = '83b89587607e3eb65c70d361f13bab43',
        sha512sum = 'fed5f6eec86144608966857f54bd69a5faa43427f27bc9178ebe2c7a1cecf925c20dbd6df07a207ae469842874efcf5b99fb7e09db59cbd92ebfc0a7e1bb'
    },
    build = {
        type    = 'gnu',
        options = { '--disable-builddir' },
        libs    = { 'ffi' },
        autoreconf = true,
        -- upstream default
        cflags = '-O3 -fomit-frame-pointer -fstrict-aliasing -ffast-math'
    }
}
