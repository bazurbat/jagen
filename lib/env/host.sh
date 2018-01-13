#!/bin/sh

pkg_install_prefix="$jagen_host_dir"

: ${pkg_build_cmake_module_path:=$jagen_host_cmake_module_path}
: ${pkg_build_cmake_module_path:=$jagen_cmake_module_path}

export CC="gcc"
export CXX="g++"
export STRIP="strip"

export CFLAGS="${pkg_build_cflags:--march=core2 -O2 -pipe}"
export CXXFLAGS="$CFLAGS"
export ASMFLAGS="$CFLAGS"
export LDFLAGS=""

export PKG_CONFIG_PATH="$jagen_host_dir/lib/pkgconfig"
