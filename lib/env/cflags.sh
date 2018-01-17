#!/bin/sh

export CFLAGS="$pkg_build_cflags"
export CXXFLAGS="${pkg_build_cxxflags:-$CFLAGS}"
export LDFLAGS="$pkg_build_ldflags"
