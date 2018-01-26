return {
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
        autoreconf = true,
        -- upstream default, should be safe
        cflags = '-O3'
    },
    install = {
        libs = { 'SoundTouch' }
    }
}
