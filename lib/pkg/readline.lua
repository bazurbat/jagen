return {
    source = 'readline-6.3.tar.gz',
    patches = {
        { "${pkg_name}-5.0-no_rpath", 0 },
        { "${pkg_name}-6.2-rlfe-tgoto", 1 },
        { "${pkg_name}-6.3-fix-long-prompt-vi-search", 1 },
        { "${pkg_name}-6.3-read-eof", 2 },
    },
    build  = {
        type = 'GNU',
        options = {
            '--disable-static',
            '--without-curses',
            '--without-purify',
        }
    }
}
