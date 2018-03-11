return {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/hi-utils.git'
    },
    build = {
        type = 'cmake'
    },
    requires = {
        'cmake-modules',
        'glib',
        'hi-sdk',
    },
    use = 'hi-sdk'
}
