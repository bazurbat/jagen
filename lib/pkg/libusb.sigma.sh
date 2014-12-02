#!/bin/sh

p_source="$pkg_dist_dir/libusb-1.0.19.tar.bz2"

use_toolchain target

p_prefix=""
p_dest_dir="$sdk_rootfs_prefix"

pkg_build() {
    p_run ./configure \
        --host="$target_system" \
        --prefix="$p_prefix" \
        --disable-static \
        --disable-udev

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$p_dest_dir" install
}
