return {
    source = {
        type     = 'git',
        location = 'git@bitbucket.org:art-system/sigma-ezboot.git',
        branch   = 'sdk4'
    },
    build = {
        type = 'sigma',
        in_source = true
    },
    use = { 'rootfs', 'xsdk' }
}
