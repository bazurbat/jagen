#!/bin/sh

export CFLAGS="-O2 -fomit-frame-pointer -fno-strict-aliasing \
    -Wa,-mips32r2 -march=24kf -mtune=24kf -pipe"
export CXXFLAGS="$CFLAGS"
export ASMFLAGS="$CFLAGS"
export LDFLAGS=""

export SMP86XX_TOOLCHAIN_PATH="${pkg_toolchain_dir:?}"
export TOOLCHAIN_RUNTIME_PATH="${pkg_toolchain_dir:?}/mips-linux-gnu/libc/el"
