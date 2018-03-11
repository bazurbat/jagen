return {
    source = {
        type      = 'dist',
        location  = 'https://matt.ucc.asn.au/dropbear/releases/dropbear-2014.66.tar.bz2',
        sha256sum = 'ab3fe2670a517cc0bbe398ff5d15e9ca12cd14f2fc18930a8111ae2baa64ab76'
    },
    build = {
        type    = 'gnu',
        options = {
            '--disable-zlib',
            '--disable-pam',
            '--enable-openpty',
            '--disable-syslog',
            '--disable-shadow',
            '--enable-bundled-libtom',
            '--disable-lastlog',
            '--disable-utmp',
            '--disable-utmpx',
            '--disable-wtmp',
            '--disable-wtmpx',
            '--disable-loginfunc',
            '--disable-pututline',
            '--disable-pututxline'
        },
        in_source = true
    },
    env = {
        -- install only the executable itself
        PROGRAMS = 'dropbear'
    }
}
