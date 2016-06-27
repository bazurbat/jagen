#!/bin/sh

# crutches ripped of from Gentoo ebuild

jagen_pkg_patch() {
    pkg_patch

    ln -s ../.. examples/rlfe/readline # for local readline headers
}

jagen_pkg_configure_target() {
    # Control cross-compiling cases when we know the right answer.
    # In cases where the C library doesn't support wide characters, readline
    # itself won't work correctly, so forcing the answer below should be OK.
    export bash_cv_func_sigsetjmp='present'
    export bash_cv_func_ctype_nonascii='yes'
    export bash_cv_wcwidth_broken='no' #503312

    pkg_configure
}
