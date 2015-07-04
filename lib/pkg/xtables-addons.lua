package {
    name   = 'xtables-addons',
    build  = {
        type    = 'GNU',
        options = '--without-kbuild',
        libs    = { 'xt_ACCOUNT_cl' }
    },
    source = 'xtables-addons-1.47.1.tar.xz',
    { 'build',
        needs = { 'xtables' }
    }
}
