#!/bin/sh

use_toolchain target

jagen_pkg_build() {
    pkg_run ./configure \
        --host="$jagen_target_system" \
        --prefix="" \
        --enable-shared \
        --disable-static

    pkg_run make
}

jagen_pkg_install() {
    pkg_run make DESTDIR="$jagen_sdk_rootfs_prefix" install
}
