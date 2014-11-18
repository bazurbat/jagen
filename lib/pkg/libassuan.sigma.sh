#!/bin/sh

p_source="$pkg_dist_dir/libassuan-2.1.2.tar.bz2"

use_env target

pkg_build() {
    p_run ./configure \
        --host="mipsel-linux" \
        --prefix=""

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$sdk_rootfs_prefix" install
    # p_fix_la "$sdk_rootfs_prefix/lib/libassuan.la"
}
