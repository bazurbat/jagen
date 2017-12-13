#!/bin/sh

pkg_system="$jagen_target_system"
pkg_prefix="/"
pkg_staging_dir="$jagen_target_dir"

: ${pkg_cmake_module_path:=$jagen_target_cmake_module_path}
: ${pkg_cmake_module_path:=$jagen_cmake_module_path}

: ${pkg_cmake_toolchain_file:=$jagen_target_cmake_toolchain_file}
: ${pkg_cmake_toolchain_file:=$jagen_cmake_toolchain_file}
: ${pkg_cmake_toolchain_file:=$(find_in_path "config/target_toolchain.cmake")}
