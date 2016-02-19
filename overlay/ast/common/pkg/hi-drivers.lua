package {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/hi-drivers.git',
        branch   = 'master'
    },
    build = {
        type = 'linux_module',
        in_source = true
    }
}
