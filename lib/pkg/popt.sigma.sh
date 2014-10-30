#!/bin/sh

p_source="$p_dist_dir/popt-1.16.tar.gz"

use_env target

pkg_build() {
    p_run ./configure \
        --host="mipsel-linux" \
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
