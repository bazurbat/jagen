#!/bin/sh

psource="libassuan-2.1.2"

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
