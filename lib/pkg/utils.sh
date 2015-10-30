#!/bin/sh

jagen_pkg_build_host() {
    pkg_run cmake -G"$jagen_cmake_generator" \
        -DCMAKE_BUILD_TYPE="$jagen_cmake_build_type" \
        -DCMAKE_INSTALL_PREFIX="$jagen_tools_dir" \
        -DUSE_LOOPAES=0 \
        ${losetup:+"-DLOSETUP=$losetup"} \
        "$pkg_source_dir"

    pkg_run cmake --build . -- $jagen_cmake_build_options
}

jagen_pkg_build_target() {
    pkg_run cmake -G"$jagen_cmake_generator" \
        -DCMAKE_BUILD_TYPE="$jagen_cmake_build_type" \
        -DCMAKE_INSTALL_PREFIX="$sdk_rootfs_root" \
        -DUSE_LOOPAES=1 \
        "$pkg_source_dir"

    pkg_run cmake --build . -- $jagen_cmake_build_options
}

jagen_pkg_install_host() {
    pkg_run cmake --build . --target install
}

jagen_pkg_install_target() {
    pkg_run cmake --build . --target install
}
