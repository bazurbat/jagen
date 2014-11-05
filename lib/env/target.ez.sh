#!/bin/sh

export target_board="ast200"

export target_dir="$sdk_rootfs_dir"
export target_prefix="/usr"

export target_arch="arm"
export target_cpu="armv7-a"
export target_system="arm-none-linux-gnueabi"

export CC="${target_system}-gcc"
export CXX="${target_system}-g++"
export STRIP="${target_system}-strip"

export CFLAGS="-O2 -mcpu=cortex-a8 -pipe"
export CXXFLAGS="$CFLAGS"
export LDFLAGS=""

export PKG_CONFIG_SYSROOT_DIR="${target_dir}${target_prefix}"
export PKG_CONFIG_LIBDIR="${PKG_CONFIG_SYSROOT_DIR}/lib/pkgconfig"
export PKG_CONFIG_PATH="${PKG_CONFIG_SYSROOT_DIR}/lib/pkgconfig"

export CROSS_MAKE="make ARCH=${target_arch} CROSS_COMPILE=${target_system}-"
