#!/bin/sh

pkg_build_host() {
    p_run cmake -G"$jagen_cmake_generator" \
        -DCMAKE_BUILD_TYPE="$jagen_cmake_build_type" \
        -DCMAKE_INSTALL_PREFIX="$jagen_tools_dir" \
        -DUSE_LOOPAES=0 \
        ${losetup:+"-DLOSETUP=$losetup"} \
        "$p_source_dir"

    p_run cmake --build . -- $jagen_cmake_build_options
}

pkg_build_target() {
    p_run cmake -G"$jagen_cmake_generator" \
        -DCMAKE_BUILD_TYPE="$jagen_cmake_build_type" \
        -DCMAKE_INSTALL_PREFIX="$sdk_rootfs_root" \
        -DUSE_LOOPAES=1 \
        "$p_source_dir"

    p_run cmake --build . -- $jagen_cmake_build_options
}

pkg_install_host() {
    p_run cmake --build . --target install
}

pkg_install_target() {
    p_run cmake --build . --target install
}
