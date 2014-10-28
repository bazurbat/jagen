#!/bin/sh

psource="popt-1.16"

use_env target

pkg_build() {
    p_cmd ./configure \
        --host="mipsel-linux" \
        --prefix="" \
        --disable-dependency-tracking \
        --enable-shared \
        --disable-static \
        --disable-nls

    p_make
}

pkg_install() {
    p_make DESTDIR="$rootfs_cross_root" install
}
