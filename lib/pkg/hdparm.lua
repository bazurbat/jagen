return {
    source = {
        type     = 'dist',
        location = 'http://downloads.sourceforge.net/project/hdparm/hdparm/hdparm-9.48.tar.gz',
        sha1sum  = '1efce086ef47667cf8016861d3ce958969e70dfc'
    },
    patches = {
        -- borrowed from Gentoo
        { 'hdparm-9.48-sysmacros', 1 },
    },
    build = {
        type = 'make',
        in_source = true,
        set_toolchain = true
    }
}
