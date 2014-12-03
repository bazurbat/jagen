#!/bin/sh

p_source="$pkg_dist_dir/libgpg-error-1.17.tar.bz2"

use_toolchain target

p_prefix=""
p_dest_dir="$sdk_rootfs_prefix"

pkg_build() {
    p_run ./configure \
        --host="$target_system" \
        --prefix="$p_prefix" \
        --disable-nls \
        --disable-rpath \
        --disable-languages

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$p_dest_dir" install
    p_fix_la "$p_dest_dir/lib/libgpg-error.la"
}
