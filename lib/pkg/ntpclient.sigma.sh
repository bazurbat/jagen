#!/bin/sh

psource="ntpclient-2010"

use_env target

pkg_build() {
    p_run sed -ri 's|^(CFLAGS.*ENABLE_DEBUG)|# \1|' Makefile
    p_run sed -ri 's|^(CFLAGS.*ENABLE_REPLAY)|# \1|' Makefile
    p_run make
}

pkg_install() {
    local dest="$rootfs_root/bin"
    p_run install -vd "$dest"
    p_run install -vm755 ntpclient "$dest"
}
