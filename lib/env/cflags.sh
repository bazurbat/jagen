#!/bin/sh

: ${pkg_toolchain_cxxflags:=$pkg_toolchain_cflags}
: ${pkg_build_cxxflags:=$pkg_build_cflags}

CFLAGS="$pkg_toolchain_cflags $CFLAGS $pkg_build_cflags"
CXXFLAGS="$pkg_toolchain_cxxflags $CXXFLAGS $pkg_build_cxxflags"
LDFLAGS="$pkg_toolchain_ldflags $LDFLAGS $pkg_build_ldflags"
export CFLAGS CXXFLAGS LDFLAGS
