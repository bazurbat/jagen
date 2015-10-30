package {
    name   = 'karaoke-player',
    build = {
        type = 'CMake',
        dir  = '$pkg_work_dir/build${pkg_config:+-$pkg_config}'
    },
    source = {
        type     = 'hg',
        location = 'ssh://hg@bitbucket.org/art-system/karaoke-player'
    }
}
