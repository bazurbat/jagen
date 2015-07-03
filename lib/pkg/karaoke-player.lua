package {
    name   = 'karaoke-player',
    build = {
        type = 'CMake',
        dir  = '$p_work_dir/build${p_config:+-$p_config}'
    },
    source = {
        type     = 'hg',
        location = 'ssh://hg@bitbucket.org/art-system/karaoke-player'
    }
}
