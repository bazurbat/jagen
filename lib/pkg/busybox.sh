#!/bin/sh

use_toolchain target

p_prefix=""
p_dest_dir="$sdk_rootfs_root"

export CROSS_COMPILE="${toolchain_bin_dir}/${target_system}-"

jagen_pkg_patch() {
    pkg_run cp -f "$jagen_private_dir/cfg/busybox.config" ".config"
}

jagen_pkg_build() {
    pkg_run make oldconfig
    pkg_run make
}

jagen_pkg_install() {
    pkg_run make CONFIG_PREFIX="$p_dest_dir" install
}
