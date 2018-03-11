return {
    source = {
        type      = 'dist',
        location  = 'https://www.cairographics.org/releases/pixman-0.32.8.tar.gz',
        sha1sum = 'c1119bbdb587c56009b653e6f81c083f98a20135'
    },
    build = {
        type    = 'gnu',
        options = {
            '--disable-static',
            '--disable-mips-dspr2',
            '--disable-gtk',
            '--disable-libpng'
        },
    },
    install = {
        libs = { 'pixman-1' }
    }
}
