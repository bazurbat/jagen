#!/bin/sh

use_env target

pkg_prefix=""
pkg_dest_dir="$jagen_sdk_rootfs_prefix"

jagen_pkg_build() {
    pkg_run "$pkg_source_dir/configure" \
        --host="$jagen_target_system" \
        --prefix="$pkg_prefix" \
        --disable-rpath \
        --disable-languages

    pkg_run make
}

jagen_pkg_install() {
    pkg_run make DESTDIR="$pkg_dest_dir" install
    pkg_fix_la "$pkg_dest_dir/lib/libgpg-error.la"
}
