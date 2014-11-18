#!/bin/sh

export host_dir="$pkg_build_dir/host"
export host_prefix=""

export PATH="$host_dir/bin:$PATH"
export LD_LIBRARY_PATH="$host_dir/lib:$LD_LIBRARY_PATH"

export CC="gcc"
export CXX="g++"
export STRIP="strip"

export CFLAGS="-march=core2 -O2 -fomit-frame-pointer -pipe"
export CXXFLAGS="$CFLAGS"
export LDFLAGS=""

unset PKG_CONFIG_SYSROOT_DIR
unset PKG_CONFIG_LIBDIR
export PKG_CONFIG_PATH="$host_dir/lib/pkgconfig"
