#!/bin/sh

use_toolchain target

p_prefix=""
p_dest_dir="$sdk_rootfs_prefix"

pkg_build() {
    pkg_run ./configure \
        --host="$target_system" \
        --prefix="$p_prefix" \
        --disable-rpath \
        --disable-languages

    pkg_run make
}

pkg_install() {
    pkg_run make DESTDIR="$p_dest_dir" install
    p_fix_la "$p_dest_dir/lib/libgpg-error.la"
}
