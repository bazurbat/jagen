#!/bin/sh

psource="expat-2.1.0"

use_env target

pkg_build() {
    p_cmd ./configure \
        --host="mipsel-linux" \
        --prefix=""

    p_make
}

pkg_install() {
    p_make DESTDIR="$rootfs_cross_root" install
}
