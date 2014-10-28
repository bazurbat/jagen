#!/bin/sh

psource="strace-4.8"

use_env target

pkg_build() {
    p_cmd ./configure \
        --host="mipsel-linux" \
        --prefix="" \
        --disable-dependency-tracking

    p_make
}

pkg_install() {
    p_make DESTDIR="$rootfs_cross_root" install
}
