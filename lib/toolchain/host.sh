#!/bin/sh

use_env host

export CC="gcc"
export CXX="g++"
export STRIP="strip"

export CFLAGS="-march=core2 -O2 -fomit-frame-pointer -pipe"
export CXXFLAGS="$CFLAGS"
export LDFLAGS=""

export PKG_CONFIG_PATH="$host_dir/lib/pkgconfig"
