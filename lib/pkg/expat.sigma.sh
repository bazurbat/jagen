#!/bin/sh

psource="expat-2.1.0"

use_env target

pkg_build() {
    p_run ./configure \
        --host="mipsel-linux" \
        --prefix=""

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$rootfs_cross_root" install
}
