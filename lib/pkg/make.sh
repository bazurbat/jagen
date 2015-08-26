#!/bin/sh

pkg_build_host() {
    p_run ./configure \
        --prefix="$tools_dir" \
        --disable-dependency-tracking \
        --disable-nls

    p_run make
}

pkg_install_host() {
    p_run make install-strip
}
