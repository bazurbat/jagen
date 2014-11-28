#!/bin/sh

p_source="$pkg_dist_dir/expat-2.1.0.tar.gz"

use_toolchain target

pkg_build() {
    p_run ./configure \
        --host="mipsel-linux" \
        --prefix=""

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$sdk_rootfs_prefix" install
}
