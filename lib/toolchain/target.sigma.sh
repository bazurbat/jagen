#!/bin/sh

use_env target

export CC="${target_system}-gcc"
export CXX="${target_system}-g++"
export STRIP="${target_system}-strip"

export CFLAGS="-O2 -fomit-frame-pointer -fno-strict-aliasing \
-Wa,-mips32r2 -march=24kf -mtune=24kf -pipe"
export CXXFLAGS="$CFLAGS"
export LDFLAGS=""
export ASMFLAGS="$CFLAGS"

export PKG_CONFIG_SYSROOT_DIR="$target_dir"
export PKG_CONFIG_LIBDIR="$target_dir$target_prefix/lib/pkgconfig"
export PKG_CONFIG_PATH="$sdk_rootfs_prefix/lib/pkgconfig"

gcc_version() { "mips-linux-gnu-gcc" --version | awk "/gcc/ { print \$NF; }"; }

check_toolchain() {
    local IFS=. version=$(gcc_version)

    read major minor patch rest <<EOF
$version
EOF

    [ "$major" = 4 ] || die "Unsupported compiler version: $version"

    if [ "$minor" -ge 4 ]; then
        pkg_flags="$pkg_flags new_toolchain"
    fi
}

check_toolchain
