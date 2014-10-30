#!/bin/sh

p_source="$p_dist_dir/gdb-7.6.2.tar.bz2"

use_env tools

pkg_prepare() {
    p_patch "05_all_readline-headers"
    p_patch "10_all_gdb-7.6-cpuid"
    p_patch "15_all_gdb-7.6-btrace"
}

pkg_build_host() {
    p_run ./configure \
        --target="mipsel-linux" \
        --prefix="$toolsdir" \
        --program-transform-name='' \
        --disable-nls \
        --with-system-readline \
        --with-expat \
        --with-python="/usr/bin/python2" \
        --with-zlib

    p_run make
}

pkg_install_host() {
    p_run make install
}
