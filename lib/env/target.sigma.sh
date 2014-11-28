#!/bin/sh

export SMP86XX_ROOTFS_PATH="$sdk_rootfs_dir"
export INSTALL_MOD_PATH="$sdk_rootfs_root"
export BDAPP_DEVICE_TABLE="$sdk_rootfs_dir/target/generic/bdapp_device_table.txt"

PATH="$sdk_rootfs_dir/host/bin:$sdk_rootfs_prefix/bin:$PATH"
