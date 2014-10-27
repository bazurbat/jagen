#!/bin/sh

export hostdir="$ja_builddir/host"
export hostprefix=""

export PATH="$hostdir/bin:$PATH"
export LD_LIBRARY_PATH="$hostdir/lib:$LD_LIBRARY_PATH"

export CC="gcc"
export CXX="g++"
export STRIP="strip"

export CFLAGS="-march=core2 -O2 -fomit-frame-pointer -pipe"
export CXXFLAGS="$CFLAGS"
export LDFLAGS=""

unset PKG_CONFIG_SYSROOT_DIR
unset PKG_CONFIG_LIBDIR
export PKG_CONFIG_PATH="$hostdir/lib/pkgconfig"
