#!/bin/sh

: ${pkg_toolchain_prefix:?}

export AR="${pkg_toolchain_prefix}ar"
export AS="${pkg_toolchain_prefix}as"
export CC="${pkg_toolchain_prefix}gcc"
export CPP="${pkg_toolchain_prefix}cpp"
export CXX="${pkg_toolchain_prefix}g++"
export LD="${pkg_toolchain_prefix}ld"
export NM="${pkg_toolchain_prefix}nm"
export OBJCOPY="${pkg_toolchain_prefix}objcopy"
export OBJDUMP="${pkg_toolchain_prefix}objdump"
export RANLIB="${pkg_toolchain_prefix}ranlib"
export STRIP="${pkg_toolchain_prefix}strip"
