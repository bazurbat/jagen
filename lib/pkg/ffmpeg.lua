package {
    name   = 'ffmpeg',
    source = 'ffmpeg-2.6.3.tar.bz2',
    build  = {
        type = 'GNU',
        dir  = '$pkg_work_dir/build${pkg_config:+-$pkg_config}'
    }
}
