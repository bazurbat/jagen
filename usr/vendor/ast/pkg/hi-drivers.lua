return {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/hi-drivers.git',
        branch   = 'master'
    },
    build = {
        type = 'linux_module',
        in_source = true
    },
    use = 'hi-kernel',
    -- hardcodes paths to SDK include dirs in Makefile
    { 'compile',
        { 'hi-sdk', 'unpack' }
    }
}
