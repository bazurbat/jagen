#!/bin/sh

jagen_pkg_configure_host() {
    pkg_run cmake -G"$jagen_cmake_generator" \
        -DCMAKE_BUILD_TYPE="$(pkg_cmake_build_type)" \
        -DCMAKE_INSTALL_PREFIX="$jagen_tools_dir" \
        -DUSE_LOOPAES=0 \
        ${losetup:+"-DLOSETUP=$losetup"} \
        "$pkg_source_dir"
}

jagen_pkg_configure_target() {
    pkg_run cmake -G"$jagen_cmake_generator" \
        -DCMAKE_BUILD_TYPE="$(pkg_cmake_build_type)" \
        -DCMAKE_INSTALL_PREFIX="$jagen_sdk_rootfs_root" \
        -DUSE_LOOPAES=1 \
        "$pkg_source_dir"
}
