package {
    name   = 'chicken-eggs',
    build = {
        type = 'manual',
        dir = '$pkg_work_dir/build${pkg_config:+-$pkg_config}'
    },
    source = {
        type     = 'git',
        location = 'https://github.com/bazurbat/chicken-eggs.git',
        branch   = 'master'
    }
}
