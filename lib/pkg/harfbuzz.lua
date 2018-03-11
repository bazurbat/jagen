return {
    source = {
        type      = 'dist',
        location  = 'https://www.freedesktop.org/software/harfbuzz/release/harfbuzz-0.9.41.tar.bz2',
        sha256sum = 'd81aa53d0c02b437beeaac159d7fc16394d676bbce0860fb6f6a10b587dc057c'
    },
    build = {
        type = 'gnu',
        options = {
            '--disable-silent-rules',
            '--disable-static',
            '--enable-shared',
            '--disable-gtk-doc',
            '--disable-gtk-doc-html',
            '--disable-gtk-doc-pdf',
            '--disable-introspection',
            '--with-glib',
            '--without-gobject',
            '--without-cairo',
            '--without-fontconfig',
            '--without-icu',
            '--without-graphite2',
            '--with-freetype',
            '--without-uniscribe',
            '--without-coretext'
        },
    },
    install = {
        libs = { 'harfbuzz' }
    },
    requires = { 'glib', 'freetype' }
}
