#!/bin/sh

use_env target

export CC="${target_system}-gcc"
export CXX="${target_system}-g++"
export STRIP="${target_system}-strip"

export CFLAGS="-O2 -fomit-frame-pointer -Wa,-mips32r2 -march=24kf -mtune=24kf -fno-strict-aliasing -pipe"
export CXXFLAGS="$CFLAGS"
export LDFLAGS=""

export PKG_CONFIG_SYSROOT_DIR="$sdk_rootfs_prefix"
export PKG_CONFIG_LIBDIR="${PKG_CONFIG_SYSROOT_DIR}/lib/pkgconfig"
export PKG_CONFIG_PATH="$target_dir$target_prefix/lib/pkgconfig"

gcc_version() { "mips-linux-gnu-gcc" --version | awk "/gcc/ { print \$NF; }"; }

check_toolchain() {
    local IFS version=$(gcc_version)
    IFS="."
    set -- $version

    [ $1 != 4 ] && die "Unsupported GCC version: $version"

    if [ $2 -ge 3 ]; then
        # CFLAGS="$CFLAGS -fno-strict-aliasing"
        # CFLAGS="$CFLAGS -Wno-unused-but-set-variable"

        pkg_flags="$pkg_flags new_toolchain"
    fi

    CXXFLAGS="$CFLAGS"
}

check_toolchain
