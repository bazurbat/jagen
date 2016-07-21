return {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/hi-sdk.git',
        branch   = 'master',
        assume_unchanged = {
            'pub/include/hi_go_bliter.h',
            'source/boot/fastboot/include/configs/s40_config.h',
            'source/msp/api/higo/include/hi_go_bliter.h',
            'source/msp/api/pvr/lib/libhi_pvrsmooth.a',
        }
    },
    build = {
        type = 'custom',
        in_source = true
    },
    requires = {
        'hi-kernel',
        'hi-sdk-tools',
    }
}
