return {
    source = {
        type      = 'dist',
        location  = 'ftp://ftp.cwru.edu/pub/bash/readline-6.3.tar.gz',
        sha256sum = '56ba6071b9462f980c5a72ab0023893b65ba6debb4eeb475d7a563dc65cafd43'
    },
    patches = {
        { "${pkg_name}-5.0-no_rpath", 0 },
        { "${pkg_name}-6.2-rlfe-tgoto", 1 },
        { "${pkg_name}-6.3-fix-long-prompt-vi-search", 1 },
        { "${pkg_name}-6.3-read-eof", 2 },
    },
    build  = {
        type = 'gnu',
        options = {
            '--disable-static',
            '--without-curses',
            '--without-purify',
        }
    }
}
