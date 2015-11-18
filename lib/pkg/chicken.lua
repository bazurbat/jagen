package {
    name   = 'chicken',
    build = {
        type = 'CMake',
        dir  = '$pkg_work_dir/build${pkg_config:+-$pkg_config}'
    },
    source = {
        type     = 'git',
        location = 'https://github.com/bazurbat/chicken-scheme.git',
        branch   = 'release-cmake'
    }
}
