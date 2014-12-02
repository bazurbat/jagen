#!/bin/sh

p_source="$pkg_dist_dir/iptables-1.4.21.tar.bz2"

use_toolchain target

p_prefix="$target_prefix"
p_dest_dir="$target_dir"

pkg_build() {
    p_run ./configure \
        --host="$target_system" \
        --prefix="$p_prefix" \
        --disable-ipv6 \
        --enable-devel \
        --disable-libipq

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$p_dest_dir" install
}
