#!/bin/sh

jagen_pkg_build_target() {
    pkg_run cmake -G"$jagen_cmake_generator" \
        -DCMAKE_TOOLCHAIN_FILE="$jagen_src_dir/android-cmake/android.toolchain.cmake" \
        -DANDROID_STANDALONE_TOOLCHAIN="${jagen_target_dir}/${target_toolchain}" \
        -DCMAKE_BUILD_TYPE="$jagen_cmake_build_type" \
        -DCMAKE_INSTALL_PREFIX="${jagen_target_dir}${jagen_target_prefix}" \
        -DHISILICON_ROOT_DIR="$jagen_sdk_dir" \
        -DHISILICON_OUT_DIR="$sdk_out_dir" \
        "$pkg_source_dir"

    pkg_run cmake --build . -- $jagen_cmake_build_options
}

jagen_pkg_install_target() {
    pkg_run cmake --build . --target install
}
