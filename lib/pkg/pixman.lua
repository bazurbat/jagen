return {
    source = {
        type      = 'dist',
        location  = 'https://www.cairographics.org/releases/pixman-0.32.8.tar.gz',
        sha256sum = '5c63dbb3523fc4d86ed4186677815918a941b7cb390d5eec4f55cb5d66b59fb1'
    },
    build = {
        type    = 'GNU',
        options = {
            '--disable-static',
            '--disable-mips-dspr2',
            '--disable-gtk',
            '--disable-libpng'
        },
        libs = { 'pixman-1' }
    }
}
