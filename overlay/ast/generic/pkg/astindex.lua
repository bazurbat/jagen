package {
    name   = 'astindex',
    build = {
        type = 'manual',
        dir = '$pkg_work_dir/build${pkg_config:+-$pkg_config}'
    },
    source = {
        type      = 'hg',
        location  = 'ssh://hg@bitbucket.org/art-system/astindex',
        directory = 'karaoke-player/source/astindex'
    }
}
