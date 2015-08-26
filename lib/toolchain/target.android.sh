#!/bin/sh

toolchain_bin_dir="${target_dir}/${target_toolchain}/bin"

export AR="${toolchain_bin_dir}/${target_system}-ar"
export CC="${toolchain_bin_dir}/${target_system}-gcc"
export CXX="${toolchain_bin_dir}/${target_system}-g++"
export STRIP="${toolchain_bin_dir}/${target_system}-strip"

export CFLAGS=""
export CXXFLAGS="$CFLAGS"
export ASMFLAGS="$CFLAGS"
export LDFLAGS=""

export PKG_CONFIG_SYSROOT_DIR="$target_dir"
export PKG_CONFIG_LIBDIR="${target_dir}${target_prefix}/lib/pkgconfig"

make_toolchain() {
    : ${jagen_toolchain_dir:?}
    "$jagen_toolchain_dir/build/tools/make-standalone-toolchain.sh" \
        --platform="$target_platform" \
        --toolchain="$target_toolchain" \
        --install-dir="${target_dir}/${target_toolchain}"
}
