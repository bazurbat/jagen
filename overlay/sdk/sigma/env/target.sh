#!/bin/sh

: ${jagen_target_system:?}

export AR="${jagen_target_system}-ar"
export CC="${jagen_target_system}-gcc"
export CXX="${jagen_target_system}-g++"
export STRIP="${jagen_target_system}-strip"

export CFLAGS="-O2 -fomit-frame-pointer -fno-strict-aliasing \
    -Wa,-mips32r2 -march=24kf -mtune=24kf -pipe"
export CXXFLAGS="$CFLAGS"
export ASMFLAGS="$CFLAGS"
export LDFLAGS=""

: ${jagen_target_dir:?}

export PKG_CONFIG_SYSROOT_DIR="${jagen_target_dir}"
export PKG_CONFIG_LIBDIR="${jagen_target_dir}/lib/pkgconfig"
export PKG_CONFIG_PATH="$jagen_sdk_rootfs_prefix/lib/pkgconfig"
