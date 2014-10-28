#!/bin/sh

psource="libassuan-2.1.2"

use_env target

pkg_build() {
    p_run ./configure \
        --host="mipsel-linux" \
        --prefix=""

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$rootfs_cross_root" install
    # p_fix_la "$rootfs_cross_root/lib/libassuan.la"
}
