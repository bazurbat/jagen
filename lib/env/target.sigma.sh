#!/bin/sh

export SMP86XX_ROOTFS_PATH="$sdk_rootfs_dir"
export INSTALL_MOD_PATH="$sdk_rootfs_root"

# for genzbf
PATH="$sdk_rootfs_prefix/bin:$PATH"
