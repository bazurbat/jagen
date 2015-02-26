#!/bin/sh

use_toolchain target

pkg_build() {
    p_run ./configure \
        --host="$target_system" \
        --prefix="" \
        --disable-maintainer-mode \
        --enable-alsatest \
        --disable-alsamixer \
        --disable-alsaconf \
        --disable-alsaloop \
        --disable-xmlto

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$sdk_rootfs_prefix" install
}
