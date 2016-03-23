package {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/hi-sample.git',
        branch   = 'master'
    },
    { 'unpack',
        { 'hi-sdk', 'unpack' }
    }
}
