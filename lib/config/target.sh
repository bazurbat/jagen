#!/bin/sh

pkg_system="$jagen_target_system"
pkg_prefix="/"
pkg_sysroot="$jagen_target_dir"

pkg_cmake_toolchain_file="${jagen_target_cmake_toolchain_file:-$jagen_cmake_toolchain_file}"
