#!/bin/sh

: ${pkg_build_system:?}

export AR="${pkg_build_system}-ar"
export AS="${pkg_build_system}-as"
export CC="${pkg_build_system}-gcc"
export CPP="${pkg_build_system}-cpp"
export CXX="${pkg_build_system}-g++"
export LD="${pkg_build_system}-ld"
export NM="${pkg_build_system}-nm"
export OBJCOPY="${pkg_build_system}-objcopy"
export OBJDUMP="${pkg_build_system}-objdump"
export RANLIB="${pkg_build_system}-ranlib"
export STRIP="${pkg_build_system}-strip"

