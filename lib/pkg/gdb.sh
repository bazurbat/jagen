#!/bin/sh

p_source="$pkg_dist_dir/gdb-7.6.2.tar.bz2"

use_env tools

pkg_prepare() {
    p_patch "05_all_readline-headers"
    p_patch "10_all_gdb-7.6-cpuid"
    p_patch "15_all_gdb-7.6-btrace"
}

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
