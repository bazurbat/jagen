#!/bin/sh

: ${pkg_install_prefix:=$jagen_host_dir}

: ${pkg_build_cmake_module_path:=$jagen_host_cmake_module_path}
: ${pkg_build_cmake_module_path:=$jagen_cmake_module_path}

CFLAGS="$pkg_toolchain_cflags -O2 $pkg_build_cflags"
CXXFLAGS="${pkg_toolchain_cxxflags:-$pkg_toolchain_cflags}"\
" -O2 ${pkg_build_cxxflags:-$pkg_build_cflags}"
LDFLAGS="$pkg_toolchain_ldflags $pkg_build_ldflags"
export CFLAGS CXXFLAGS LDFLAGS

export PKG_CONFIG_PATH="$pkg_install_prefix/lib/pkgconfig"
