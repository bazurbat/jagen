#!/bin/sh

p_source="$pkg_dist_dir/busybox-1.22.1.tar.bz2"

use_toolchain target

p_prefix=""
p_dest_dir="$sdk_rootfs_root"

pkg_build() {
    export CROSS_COMPILE="${target_system}-"

    p_run cp -f "$pkg_private_dir/cfg/busybox.config" ".config"
    p_run make oldconfig
    p_run make
}

pkg_install() {
    p_run install -vm755 busybox "$p_dest_dir/bin/busybox"
}
