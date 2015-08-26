package {
    name   = 'utils',
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/sigma-utils.git'
    },
    build = {
        type = 'CMake',
        dir  = '$p_work_dir/build${p_config:+-$p_config}'
    }
}
