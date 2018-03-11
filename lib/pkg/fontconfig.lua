return {
    source = {
        type      = 'dist',
        location  = 'https://www.freedesktop.org/software/fontconfig/release/fontconfig-2.11.1.tar.bz2',
        sha256sum = 'dc62447533bca844463a3c3fd4083b57c90f18a70506e7a9f4936b5a1e516a99'
    },
    build  = {
        type = 'gnu',
        options = {
            '--disable-static',
            '--enable-shared',
            '--enable-iconv',
            '--disable-libxml2',
            '--disable-docs',
        },
    },
    install = {
        libs = { 'fontconfig' }
    },
    requires = { 'expat', 'freetype' }
}
