#!/bin/sh

use_toolchain target

jagen_pkg_build() {
    pkg_run ./configure \
        --host="$jagen_target_system" \
        --prefix="" \
        --disable-maintainer-mode \
        --enable-alsatest \
        --disable-alsamixer \
        --disable-alsaconf \
        --disable-alsaloop \
        --disable-xmlto

    pkg_run make
}

jagen_pkg_install() {
    pkg_run make DESTDIR="$sdk_rootfs_prefix" install
}
