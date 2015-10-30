package {
    name   = 'ast-files',
    build = {
        dir = '$pkg_work_dir/build${pkg_config:+-$pkg_config}'
    },
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/files.git'
    }
}
