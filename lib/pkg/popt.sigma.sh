#!/bin/sh

use_toolchain target

pkg_build() {
    p_run ./configure \
        --host="$target_system" \
        --prefix="" \
        --disable-dependency-tracking \
        --enable-shared \
        --disable-static \
        --disable-nls

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$sdk_rootfs_prefix" install
}
