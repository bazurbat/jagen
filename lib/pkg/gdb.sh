#!/bin/sh

pkg_build_host() {
    p_run ./configure \
        --target="$target_system" \
        --prefix="$tools_dir" \
        --program-transform-name='' \
        --disable-werror \
        --disable-binutils \
        --disable-etc \
        --disable-gas \
        --disable-gold \
        --disable-gprof \
        --disable-ld \
        --disable-gdbserver \
        --disable-readline \
        --with-system-readline \
        --with-expat \
        --with-python="/usr/bin/python2.7" \
        --with-zlib

    p_run make
}

pkg_install_host() {
    p_run make install
}
