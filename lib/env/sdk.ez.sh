#!/bin/sh

export target_dir="/src/nfs/rootfs"
export target_prefix="/usr"

export target_arch="arm"
export target_cpu="armv7-a"
export target_system="arm-none-linux-gnueabi"

export target_board="ast200"

export sdk_rootfs_dir="$target_dir"

export EZSDK="$pkg_src_dir/ez"
export LINUX_KERNEL="$pkg_src_dir/linux"
export INSTALL_MOD_PATH="$sdk_rootfs_dir"

export kernel_version="2.6.37+"
export kernel_release="$kernel_version"
export kernel_modules_dir="$INSTALL_MOD_PATH/lib/modules/$kernel_release"

export sdk_rules="$EZSDK/Rules.make"
