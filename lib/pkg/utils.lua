package {
    name   = 'utils',
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/sigma-utils.git'
    },
    build = {
        type = 'CMake',
        dir  = '$pkg_work_dir/build${pkg_config:+-$pkg_config}'
    }
}
