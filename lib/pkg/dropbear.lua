package {
    source = 'dropbear-2014.66.tar.bz2',
    build = {
        type    = 'GNU',
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
            '--disable-pututlinux',
            '--disable-pututxline'
        },
        in_source = true
    }
}