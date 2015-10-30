#!/bin/sh

use_toolchain target

pkg_build() {
    pkg_run ./configure \
        --host="$target_system" \
        --prefix="" \
        --enable-shared \
        --disable-static

    pkg_run make
}

pkg_install() {
    pkg_run make DESTDIR="$sdk_rootfs_prefix" install
}
