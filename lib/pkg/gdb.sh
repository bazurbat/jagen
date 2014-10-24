#!/bin/sh

psource="gdb-7.6.2"

pkg_prepare() {
    p_patch "05_all_readline-headers" \
        >"$plog" 2>&1 || return $?
    p_patch "10_all_gdb-7.6-cpuid" \
        >>"$plog" 2>&1 || return $?
    p_patch "15_all_gdb-7.6-btrace" \
        >>"$plog" 2>&1 || return $?
}

pkg_build_host() {
    p_cmd ./configure \
        --target="mipsel-linux" \
        --prefix="$toolsdir" \
        --program-transform-name='' \
        --disable-nls \
        --with-system-readline \
        --with-expat \
        --with-python="/usr/bin/python2" \
        --with-zlib

    p_make
}

pkg_install_host() {
    p_make install
}
