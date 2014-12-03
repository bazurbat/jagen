#!/bin/sh

p_source="$pkg_dist_dir/soundtouch-1.8.0.tar.gz"
p_source_dir="$p_work_dir/soundtouch"

use_toolchain target

p_prefix="$target_prefix"
p_dest_dir="$target_dir"

pkg_patch() {
    p_run ./bootstrap
}

pkg_build() {
    p_run ./configure \
        --host="$target_system" \
        --prefix="$p_prefix" \
        --enable-integer-samples=yes \
        --enable-x86-optimizations=no

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$p_dest_dir" install
    p_fix_la "$p_dest_dir$p_prefix/lib/libSoundTouch.la" "$p_dest_dir"
}
