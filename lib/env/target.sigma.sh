#!/bin/sh

export targetdir="$ja_builddir/target"
export targetprefix="/firmware"

export target_arch="mipsel"
export target_cpu="24kf"
export target_system="mipsel-linux"

export CC="${target_system}-gcc"
export CXX="${target_system}-g++"
export STRIP="${target_system}-strip"

export CFLAGS="-O2 -fomit-frame-pointer -Wa,-mips32r2 -march=24kf -mtune=24kf -pipe"
export CXXFLAGS="$CFLAGS"
export LDFLAGS=""

export PKG_CONFIG_SYSROOT_DIR="$rootfsdir/cross_rootfs"
export PKG_CONFIG_LIBDIR="${PKG_CONFIG_SYSROOT_DIR}/lib/pkgconfig"
export PKG_CONFIG_PATH="$targetdir/firmware/lib/pkgconfig"

SMP86XX_ROOTFS_PATH="$rootfsdir"
INSTALL_MOD_PATH="$SMP86XX_ROOTFS_PATH/build_mipsel/root"
BDAPP_DEVICE_TABLE="$SMP86XX_ROOTFS_PATH/target/generic/bdapp_device_table.txt"
export SMP86XX_ROOTFS_PATH INSTALL_MOD_PATH BDAPP_DEVICE_TABLE

# fixes: undefined reference to `rpl_malloc'
ac_cv_func_malloc_0_nonnull=yes
export ac_cv_func_malloc_0_nonnull

PATH="$SMP86XX_ROOTFS_PATH/host/bin:$SMP86XX_ROOTFS_PATH/cross_rootfs/bin:$PATH"
