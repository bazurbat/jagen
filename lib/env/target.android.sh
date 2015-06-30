#!/bin/sh

target_arch="arm"
target_system="arm-linux-androideabi"
target_platform="${target_platform:-android-17}"
target_toolchain="${target_toolchain:-${target_system}-4.6}"

target_dir="$pkg_build_dir/target"
target_prefix="/system"

target_bin_dir="$target_dir/$target_toolchain"
