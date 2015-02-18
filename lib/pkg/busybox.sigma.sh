#!/bin/sh

use_toolchain target

p_prefix=""
p_dest_dir="$sdk_rootfs_root"

export CROSS_COMPILE="${target_system}-"

pkg_patch() {
    p_run cp -f "$pkg_private_dir/cfg/busybox.config" ".config"
}

pkg_build() {
    p_run make oldconfig
    p_run make
}

pkg_install() {
    p_run make CONFIG_PREFIX="$p_dest_dir" install
}
