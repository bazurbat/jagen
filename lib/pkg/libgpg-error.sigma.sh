#!/bin/sh

psource="libgpg-error-1.17"

use_env target

pkg_build() {
    p_run ./configure \
        --host="mipsel-linux" \
        --prefix="" \
        --disable-nls \
        --disable-rpath \
        --disable-languages

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$rootfs_cross_root" install
    # p_fix_la "$rootfs_cross_root/lib/libgpg-error.la"
}
