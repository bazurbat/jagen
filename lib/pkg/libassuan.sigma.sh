#!/bin/sh

psource="libassuan-2.1.2"

use_env target

pkg_build() {
    p_cmd ./configure \
        --host="mipsel-linux" \
        --prefix=""

    p_cmd make
}

pkg_install() {
    p_cmd make DESTDIR="$rootfs_cross_root" install
    # p_fix_la "$rootfs_cross_root/lib/libassuan.la"
}
