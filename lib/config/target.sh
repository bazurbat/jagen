#!/bin/sh

pkg_system="$jagen_target_system"
pkg_install_prefix="/"
pkg_install_root="$jagen_target_dir"

: ${pkg_build_cmake_module_path:=$jagen_target_cmake_module_path}
: ${pkg_build_cmake_module_path:=$jagen_cmake_module_path}

: ${pkg_build_cmake_toolchain_file:=$jagen_target_cmake_toolchain_file}
: ${pkg_build_cmake_toolchain_file:=$jagen_cmake_toolchain_file}
