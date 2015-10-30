#!/bin/sh

use_toolchain target

pkg_build() {
    pkg_run ./configure \
        --host="$target_system" \
        --prefix="" \
        --disable-maintainer-mode \
        --enable-alsatest \
        --disable-alsamixer \
        --disable-alsaconf \
        --disable-alsaloop \
        --disable-xmlto

    pkg_run make
}

pkg_install() {
    pkg_run make DESTDIR="$sdk_rootfs_prefix" install
}
