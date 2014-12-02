#!/bin/sh

p_source="$pkg_dist_dir/xtables-addons-1.47.1.tar.xz"

use_toolchain target

p_prefix="$target_prefix"
p_dest_dir="$target_dir"

pkg_build() {
    p_run ./configure \
        --host="$target_system" \
        --prefix="$p_prefix" \
        --without-kbuild

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$p_dest_dir" install
}
