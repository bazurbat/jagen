package {
    name   = 'xtables-addons',
    source = 'xtables-addons-1.47.1.tar.xz',
    build  = {
        type    = 'GNU',
        options = { '--without-kbuild' },
        libs    = { 'xt_ACCOUNT_cl' }
    }
}
