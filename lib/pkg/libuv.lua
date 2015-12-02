package {
    name   = 'libuv',
    build  = {
        type    = 'GNU',
        dir     = '$pkg_work_dir/build${pkg_config:+-$pkg_config}',
        options = '--disable-static',
        libs    = { 'uv' }
    },
    source = 'libuv-1.4.2.tar.gz'
}
