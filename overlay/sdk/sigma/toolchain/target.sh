#!/bin/sh

export CFLAGS="-O2 -fomit-frame-pointer -fno-strict-aliasing \
    -Wa,-mips32r2 -march=24kf -mtune=24kf -pipe"
export CXXFLAGS="$CFLAGS"
export ASMFLAGS="$CFLAGS"
export LDFLAGS=""

export PKG_CONFIG_PATH="$jagen_sdk_rootfs_prefix/lib/pkgconfig"
