return {
    source = {
        type      = 'dist',
        location  = 'https://github.com/libass/libass/releases/download/0.12.2/libass-0.12.2.tar.xz',
        sha256sum = '673c1b15bde182168b17e17ee1c8d9e01173f20af352134216135b5bf15bf1e3'
    },
    build  = {
        type    = 'gnu',
        options = {
            '--disable-static',
            '--disable-enca',
            '--disable-fontconfig',
            '--disable-harfbuzz'
        },
        libs    = { 'ass' }
    },
    requires = { 'freetype', 'fribidi' }
}
