#!/bin/sh

export CFLAGS="$toolchain_cflags $CFLAGS $pkg_build_cflags"
export CXXFLAGS="$toolchain_cxxflags $CXXFLAGS $pkg_build_cxxflags"
export LDFLAGS="$toolchain_ldflags $LDFLAGS $pkg_build_ldflags"
