package {
    name   = 'chicken-eggs',
    build = {
        dir = '$p_work_dir/build${p_config:+-$p_config}'
    },
    source = {
        type     = 'git',
        location = 'https://github.com/bazurbat/chicken-eggs.git',
        branch   = 'master'
    }
}
