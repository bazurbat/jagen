#!/bin/sh

use_env target

export CC="${target_system}-gcc"
export CXX="${target_system}-g++"
export STRIP="${target_system}-strip"

export CFLAGS="-O2 -fomit-frame-pointer -Wa,-mips32r2 -march=24kf -mtune=24kf -pipe"
export CXXFLAGS="$CFLAGS"
export LDFLAGS=""

export PKG_CONFIG_SYSROOT_DIR="$sdk_rootfs_prefix"
export PKG_CONFIG_LIBDIR="${PKG_CONFIG_SYSROOT_DIR}/lib/pkgconfig"
export PKG_CONFIG_PATH="$target_dir$target_prefix/lib/pkgconfig"

gcc_version() { "$1" --version | awk "/gcc/ { print \$NF; }"; }

adjust_cflags() {
    local IFS version=$(gcc_version "$CC")
    IFS="."
    set -- $version

    [ $1 != 4 ] && die "Unsupported GCC version: $version"

    if [ $2 -ge 6 ]; then
        CFLAGS="$CFLAGS -fno-strict-aliasing"
        CFLAGS="$CFLAGS -Wno-unused-but-set-variable"
    fi

    CXXFLAGS="$CFLAGS"
}

adjust_cflags
