package {
    name   = 'ast-files',
    build = {
        dir = '$p_work_dir/build${p_config:+-$p_config}'
    },
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/files.git'
    }
}
