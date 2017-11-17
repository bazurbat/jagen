#!/bin/sh

export CC="gcc"
export CXX="g++"
export STRIP="strip"

export CFLAGS="${jagen_host_cflags:--march=core2 -O2 -pipe}"
export CXXFLAGS="$CFLAGS"
export ASMFLAGS="$CFLAGS"
export LDFLAGS=""

export PKG_CONFIG_PATH="$jagen_host_dir/lib/pkgconfig"
