#!/bin/sh

use_env target

[ "$android_toolchain_dir" ] ||
    die "android_toolchain_dir is not set"

export AR="${android_toolchain_dir}/bin/${target_system}-ar"
export CC="${android_toolchain_dir}/bin/${target_system}-gcc"
export CXX="${android_toolchain_dir}/bin/${target_system}-g++"
export STRIP="${android_toolchain_dir}/bin/${target_system}-strip"

# export CFLAGS="-O2 -fomit-frame-pointer -fno-strict-aliasing -pipe"
export CFLAGS=""
export CXXFLAGS="$CFLAGS"
export ASMFLAGS="$CFLAGS"
export LDFLAGS=""

export PKG_CONFIG_SYSROOT_DIR="$target_dir"
export PKG_CONFIG_LIBDIR="$target_dir$target_prefix/lib/pkgconfig"
# export PKG_CONFIG_PATH="$sdk_rootfs_prefix/lib/pkgconfig"
