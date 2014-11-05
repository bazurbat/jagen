#!/bin/sh

export EZSDK="$ja_src_dir/ez"

export sdk_rootfs_dir="/srv/nfs/rootfs"
export kernel_version="2.6.37+"

export LINUX_KERNEL="$ja_src_dir/linux"
export INSTALL_MOD_PATH="$sdk_rootfs_dir"
