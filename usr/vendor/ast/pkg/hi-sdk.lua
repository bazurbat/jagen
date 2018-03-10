return {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/hi-sdk.git',
        assume_unchanged = {
            'pub/include/hi_go_bliter.h',
            'source/boot/fastboot/include/configs/s40_config.h',
            'source/msp/api/higo/include/hi_go_bliter.h',
            'source/msp/api/pvr/lib/libhi_pvrsmooth.a',
        }
    },
    build = {
        type = true,
        in_source = true,
        unset_cflags = true,
        kernel_modules = true
    },
    use = 'kernel'
}
