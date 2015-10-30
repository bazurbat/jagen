#!/bin/sh

use_toolchain target

pkg_build() {
    pkg_run sed -ri 's|^(CFLAGS.*ENABLE_DEBUG)|# \1|' Makefile
    pkg_run sed -ri 's|^(CFLAGS.*ENABLE_REPLAY)|# \1|' Makefile
    pkg_run make
}

pkg_install() {
    local dest="$sdk_rootfs_root/bin"
    pkg_run install -vd "$dest"
    pkg_run install -vm755 ntpclient "$dest"
}
