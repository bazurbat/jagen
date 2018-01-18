#!/bin/sh

pkg_install_prefix=$jagen_host_dir

if [ "$jagen_host_cmake_module_path" ]; then
    jagen_cmake_module_path=$jagen_host_cmake_module_path
fi

export CFLAGS="-O2"
export CXXFLAGS="$CFLAGS"
export LDFLAGS=""

export PKG_CONFIG_PATH="$pkg_install_prefix/lib/pkgconfig"
