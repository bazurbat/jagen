package {
    name   = 'chicken',
    build = {
        dir = '$p_work_dir/build${p_config:+-$p_config}'
    },
    source = {
        type     = 'git',
        location = 'https://github.com/bazurbat/chicken-scheme.git',
        branch   = 'cmake'
    }
}
