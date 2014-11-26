#!/bin/sh

p_source="$pkg_dist_dir/iptables-1.4.21.tar.bz2"

use_env target

pkg_build() {
    p_run ./configure \
        --host="$target_system" \
        --prefix="$target_prefix" \
        --disable-ipv6 \
        --enable-devel \
        --disable-libipq \
        --with-kernel="$LINUX_KERNEL"

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$target_dir" install
}
