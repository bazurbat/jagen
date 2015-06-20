package {
    name    = 'sqlite',
    source  = 'sqlite-autoconf-3080403.tar.gz',
    patches = {
        { 'sqlite-3.8.1-autoconf-dlopen_check', 0 }
    }
}
