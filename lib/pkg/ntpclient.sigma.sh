#!/bin/sh

psource="ntpclient-2010"

use_env target

pkg_build() {
    p_cmd sed -ri 's|^(CFLAGS.*ENABLE_DEBUG)|# \1|' Makefile
    p_cmd sed -ri 's|^(CFLAGS.*ENABLE_REPLAY)|# \1|' Makefile
    p_cmd make
}

pkg_install() {
    local dest="$rootfs_root/bin"
    p_cmd install -vd "$dest"
    p_cmd install -vm755 ntpclient "$dest"
}
