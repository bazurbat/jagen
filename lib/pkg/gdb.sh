#!/bin/sh

p_source="$pkg_dist_dir/gdb-7.6.2.tar.bz2"

use_env tools

pkg_build_host() {
    p_run ./configure \
        --target="$target_system" \
        --prefix="$tools_dir" \
        --program-transform-name='' \
        --disable-nls \
        --with-system-readline \
        --with-expat \
        --with-python="/usr/bin/python2.7" \
        --with-zlib

    p_run make
}

pkg_install_host() {
    p_run make install
}
