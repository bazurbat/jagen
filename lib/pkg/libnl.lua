return {
    source = {
        type      = 'dist',
        location  = 'https://www.infradead.org/~tgr/libnl/files/libnl-3.2.25.tar.gz',
        sha256sum = '8beb7590674957b931de6b7f81c530b85dc7c1ad8fbda015398bc1e8d1ce8ec5'
    },
    patches = {
        { 'libnl-3.2.20-rtnl_tc_get_ops', 1 },
        { 'libnl-3.2.20-cache-api',       1 },
    },
    build = {
        type    = 'gnu',
        options = {
            '--enable-shared',
            '--disable-static',
            '--disable-cli'
        },
    },
    install = {
        libs = { 'nl-3', 'nl-genl-3', 'nl-route-3', 'nl-nf-3', 'nl-idiag-3' }
    }
}
