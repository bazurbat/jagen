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

if [ -z "$jagen_target_toolchain_dir" ]; then
    jagen_target_toolchain_dir="$jagen_toolchain_dir/bin"
fi

if [ -z "$jagen_toolchain_prefix" ]; then
    : ${jagen_target_system:?}
    jagen_toolchain_prefix="${jagen_target_toolchain_dir}/${jagen_target_system}-"
fi

if [ -d "$jagen_target_toolchain_dir" ]; then
    add_PATH "$jagen_target_toolchain_dir"
fi
