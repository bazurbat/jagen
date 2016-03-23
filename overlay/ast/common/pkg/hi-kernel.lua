package {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/hi-kernel.git',
        branch   = 'master'
    },
    { 'unpack',
        { 'hi-sdk', 'unpack' }
    }
}
