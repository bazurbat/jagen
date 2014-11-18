#!/bin/sh

p_source="$pkg_dist_dir/make-3.80.tar.bz2"

pkg_build_host() {
    use_env tools

    p_run ./configure \
        --prefix="$tools_dir" \
        --disable-dependency-tracking \
        --disable-nls

    p_run make
}

pkg_install_host() {
    p_run make install-strip
}
