return {
    source = {
        type     = 'dist',
        location = 'http://downloads.sourceforge.net/project/xtables-addons/Xtables-addons/xtables-addons-1.47.1.tar.xz',
        sha1sum  = '771bd818345a8569606724f157544a54ac3c0245'
    },
    build  = {
        type    = 'gnu',
        options = { '--without-kbuild' },
        libs    = { 'xt_ACCOUNT_cl' }
    },
    requires = { 'xtables' }
}
