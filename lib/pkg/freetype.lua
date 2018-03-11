return {
    source = {
        type      = 'dist',
        location  = 'http://download.savannah.gnu.org/releases/freetype/freetype-2.5.0.1.tar.bz2',
        sha256sum = '57bce5b37989577aa8b4a588426839f6bf39bcc3869748cb18f6827df251f4e5'
    },
    patches = {
        { 'freetype-2.3.2-enable-valid',   1 },
        { 'freetype-2.4.11-sizeof-types',  1 },
        { 'freetype-2.4.12-clean-include', 1 }
    },
    build = {
        type    = 'gnu',
        options = {
            '--disable-static',
            '--without-bzip2',
            '--without-png',
            '--without-old-mac-fonts',
            '--without-fsspec',
            '--without-fsref',
            '--without-quickdraw-toolbox',
            '--without-quickdraw-carbon',
            '--without-ats'
        },
    },
    install = {
        libs = { 'freetype' }
    }
}
