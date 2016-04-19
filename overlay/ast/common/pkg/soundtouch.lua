rule {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/soundtouch.git',
        branch   = 'master'
    },
    build = {
        type = 'GNU',
        options = {
            '--enable-integer-samples=yes',
            '--enable-x86-optimizations=no'
        },
        libs = { 'SoundTouch' },
        autoreconf = true
    }
}
