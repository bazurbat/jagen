#!/bin/sh

use_env target

export CC="${target_system}-gcc"
export CXX="${target_system}-g++"
export STRIP="${target_system}-strip"

export CFLAGS="-O2 -fomit-frame-pointer -fno-strict-aliasing -pipe"
export CXXFLAGS="$CFLAGS"
export LDFLAGS=""
export ASMFLAGS="$CFLAGS"

export PKG_CONFIG_SYSROOT_DIR="$target_dir"
export PKG_CONFIG_LIBDIR="$target_dir$target_prefix/lib/pkgconfig"
# export PKG_CONFIG_PATH="$sdk_rootfs_prefix/lib/pkgconfig"
