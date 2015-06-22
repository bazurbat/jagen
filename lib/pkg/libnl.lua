package {
    name    = 'libnl',
    build = {
        type    = 'GNU',
        options = '--enable-shared --disable-static --disable-cli',
        libs    = { 'nl-3', 'nl-genl-3', 'nl-route-3', 'nl-nf-3', 'nl-idiag-3' }
    },
    source  = 'libnl-3.2.25.tar.gz',
    patches = {
        { 'libnl-3.2.20-rtnl_tc_get_ops', 1 },
        { 'libnl-3.2.20-cache-api',       1 },
    }
}
