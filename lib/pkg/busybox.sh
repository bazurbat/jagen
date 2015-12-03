#!/bin/sh

use_toolchain target

pkg_prefix=""
pkg_dest_dir="$sdk_rootfs_root"

export CROSS_COMPILE="${toolchain_dir}/bin/${jagen_target_system}-"

jagen_pkg_patch() {
    pkg_run cp -f "$jagen_private_dir/cfg/busybox.config" ".config"
}

jagen_pkg_build() {
    pkg_run make oldconfig
    pkg_run make
}

jagen_pkg_install() {
    pkg_run make CONFIG_PREFIX="$pkg_dest_dir" install
}
