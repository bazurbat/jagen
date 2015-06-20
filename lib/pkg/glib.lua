package {
    name    = 'glib',
    source  = 'glib-2.40.2.tar.xz',
    patches = {
        { 'glib-2.40.0-external-gdbus-codegen', 1 }
    }
}
