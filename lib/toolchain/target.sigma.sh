#!/bin/sh

use_env target

export CC="${target_system}-gcc"
export CXX="${target_system}-g++"
export STRIP="${target_system}-strip"

cflags_optimize="-O2 -fomit-frame-pointer -fno-strict-aliasing"
cflags_tune="-Wa,-mips32r2 -march=24kf -mtune=24kf"
cflags_paths="-isystem $sdk_rootfs_prefix/include"

export CFLAGS="$cflags_optimize $cflags_tune $cflags_paths -pipe"
export CXXFLAGS="$CFLAGS"
export LDFLAGS="-L$sdk_rootfs_prefix/lib"

export PKG_CONFIG_SYSROOT_DIR="$target_dir"
export PKG_CONFIG_LIBDIR="$target_dir$target_prefix/lib/pkgconfig"
export PKG_CONFIG_PATH="$sdk_rootfs_prefix/lib/pkgconfig"

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
