#!/bin/sh

export CC="gcc"
export CXX="g++"
export STRIP="strip"

export CFLAGS="-march=core2 -O2 -fomit-frame-pointer -pipe"
export CXXFLAGS="$CFLAGS"
export LDFLAGS=""
export ASMFLAGS="$CFLAGS"

export PKG_CONFIG_PATH="$jagen_host_dir/lib/pkgconfig"
