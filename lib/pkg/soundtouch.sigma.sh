#!/bin/sh

p_source="$p_dist_dir/soundtouch-1.8.0.tar.gz"
p_source_dir="$p_work_dir/soundtouch"

use_env target

pkg_prepare() {
    p_run ./bootstrap
}

pkg_build() {
    p_run ./configure \
        --host="mipsel-linux" \
        --prefix="$target_prefix" \
        --enable-integer-samples=yes \
        --enable-x86-optimizations=no

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$target_dir" install
}
