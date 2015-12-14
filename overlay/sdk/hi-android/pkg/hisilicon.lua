package {
    name   = 'hisilicon',
    source = {
        type     = 'git',
        location = 'builder@10.0.2.1:/var/data/public/src/hisilicon',
        branch   = 'master'
    },
    build = {
        type = 'make',
        directory = '$pkg_work_dir'
    }
}
