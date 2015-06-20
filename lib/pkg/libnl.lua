package {
    name    = 'libnl',
    source  = 'libnl-3.2.25.tar.gz',
    patches = {
        { 'libnl-3.2.20-rtnl_tc_get_ops', 1 },
        { 'libnl-3.2.20-cache-api',       1 },
    }
}
