#!/bin/sh

export target_board="ast50"

export target_dir="$ja_build_dir/target"
export target_prefix="/firmware"

export target_arch="mipsel"
export target_cpu="24kf"
export target_system="mipsel-linux"

export CC="${target_system}-gcc"
export CXX="${target_system}-g++"
export STRIP="${target_system}-strip"

export CFLAGS="-O2 -fomit-frame-pointer -Wa,-mips32r2 -march=24kf -mtune=24kf -pipe"
export CXXFLAGS="$CFLAGS"
export LDFLAGS=""

export PKG_CONFIG_SYSROOT_DIR="$sdk_rootfs_prefix"
export PKG_CONFIG_LIBDIR="${PKG_CONFIG_SYSROOT_DIR}/lib/pkgconfig"
export PKG_CONFIG_PATH="$target_dir$target_prefix/lib/pkgconfig"

export SMP86XX_ROOTFS_PATH="$sdk_rootfs_dir"
export INSTALL_MOD_PATH="$sdk_rootfs_root"
export BDAPP_DEVICE_TABLE="$sdk_rootfs_dir/target/generic/bdapp_device_table.txt"

# fixes: undefined reference to `rpl_malloc'
ac_cv_func_malloc_0_nonnull=yes
export ac_cv_func_malloc_0_nonnull

PATH="$sdk_rootfs_dir/host/bin:$sdk_rootfs_prefix/bin:$PATH"
