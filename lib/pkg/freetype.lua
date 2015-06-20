package {
    name    = 'freetype',
    source  = 'freetype-2.5.0.1.tar.bz2',
    patches = {
        { 'freetype-2.3.2-enable-valid',   1 },
        { 'freetype-2.4.11-sizeof-types',  1 },
        { 'freetype-2.4.12-clean-include', 1 }
    }
}
