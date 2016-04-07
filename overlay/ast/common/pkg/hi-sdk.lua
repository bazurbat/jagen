package {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/hi-sdk.git',
        branch   = 'master',
        assume_unchanged = {
            'pub/include/hi_go_bliter.h',
            'source/msp/api/higo/include/hi_go_bliter.h',
            'source/msp/api/pvr/lib/libhi_pvrsmooth.a',
        }
    },
    build = {
        in_source = true
    }
}
