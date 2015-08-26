#!/bin/sh

export AR="${target_system}-ar"
export CC="${target_system}-gcc"
export CXX="${target_system}-g++"
export STRIP="${target_system}-strip"

export CFLAGS=""
export CXXFLAGS="${CFLAGS}"
export ASMFLAGS="${CFLAGS}"
export LDFLAGS=""

export PKG_CONFIG_SYSROOT_DIR="${target_dir}"
export PKG_CONFIG_LIBDIR="${target_dir}${target_prefix}/lib/pkgconfig"

add_PATH "${toolchain_bin_dir}"
