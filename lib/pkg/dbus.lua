package {
    name   = 'dbus',
    source = 'dbus-1.6.18.tar.gz',
    build  = {
        type = 'GNU',
        libs = { 'dbus-1' }
    },
    { 'build',
        needs = { 'expat' }
    }
}
