#!/bin/sh

p_source="$p_dist_dir/strace-4.8.tar.xz"

use_env target

pkg_build() {
    p_run ./configure \
        --host="mipsel-linux" \
        --prefix="" \
        --disable-dependency-tracking

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$sdk_rootfs_prefix" install
}
