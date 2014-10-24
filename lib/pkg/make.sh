#!/bin/sh

psource="make-3.80"

pkg_build_host() {
    use_env tools

    p_cmd ./configure \
        --prefix="$toolsdir" \
        --disable-dependency-tracking \
        --disable-nls

    p_make
}

pkg_install_host() {
    p_make install-strip
}
