#!/bin/sh

: ${jagen_target_system:?}

export AR="${jagen_target_system}-ar"
export CC="${jagen_target_system}-gcc"
export CXX="${jagen_target_system}-g++"
export STRIP="${jagen_target_system}-strip"

export CFLAGS=""
export CXXFLAGS="${CFLAGS}"
export ASMFLAGS="${CFLAGS}"
export LDFLAGS=""

: ${jagen_target_dir:?}

export PKG_CONFIG_SYSROOT_DIR="${jagen_target_dir}"
export PKG_CONFIG_LIBDIR="${jagen_target_dir}${jagen_target_prefix}/lib/pkgconfig"

: ${jagen_target_toolchain_dir:?}

add_PATH "$jagen_target_toolchain_dir"
