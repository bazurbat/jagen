#!/bin/sh

use_env target

export AR="${target_bin_dir}/bin/${target_system}-ar"
export CC="${target_bin_dir}/bin/${target_system}-gcc"
export CXX="${target_bin_dir}/bin/${target_system}-g++"
export STRIP="${target_bin_dir}/bin/${target_system}-strip"

export CFLAGS=""
export CXXFLAGS="$CFLAGS"
export ASMFLAGS="$CFLAGS"
export LDFLAGS=""

export PKG_CONFIG_SYSROOT_DIR="$target_dir"
export PKG_CONFIG_LIBDIR="$target_dir$target_prefix/lib/pkgconfig"

make_toolchain() {
    "$jagen_toolchain_dir/build/tools/make-standalone-toolchain.sh" \
        --platform="$target_platform" \
        --toolchain="$target_toolchain" \
        --install-dir="$target_bin_dir"
}
