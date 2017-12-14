#!/bin/sh

pkg_system="$jagen_host_system"
pkg_install_prefix="$jagen_host_dir"

: ${pkg_build_cmake_module_path:=$jagen_host_cmake_module_path}
: ${pkg_build_cmake_module_path:=$jagen_cmake_module_path}
