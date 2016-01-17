#!/bin/sh

jagen_pkg_build_target() {
    local IFS="$(printf '\n\t')"
    local S="$(printf '\t')"
    local A="-G$jagen_cmake_generator"
    A="$A$S-DCMAKE_MODULE_PATH=$jagen_src_dir/cmake-modules"
    A="$A$S-DCMAKE_SYSTEM_NAME=Linux"
    A="$A$S-DCMAKE_BUILD_TYPE=$jagen_cmake_build_type"
    A="$A$S-DCMAKE_INSTALL_PREFIX=${jagen_target_dir}${jagen_target_prefix}"

    case $jagen_sdk in
        android)
            pkg_run cmake $A \
                -DCMAKE_MODULE_PATH="$jagen_src_dir/cmake-modules" \
                -DCMAKE_TOOLCHAIN_FILE="$jagen_src_dir/android-cmake/android.toolchain.cmake" \
                -DANDROID_STANDALONE_TOOLCHAIN="${jagen_target_dir}/${jagen_target_toolchain}" \
                -DHISILICON_ROOT_DIR="$jagen_sdk_dir" \
                -DHISILICON_OUT_DIR="$jagen_install_dir" \
                "$pkg_source_dir"
            ;;
        hi-linux)
            pkg_run cmake $A \
                -DHISILICON_ROOT_DIR="$jagen_sdk_dir" \
                "$pkg_source_dir"
            ;;
    esac

    pkg_run cmake --build . -- $jagen_cmake_build_options
}

jagen_pkg_install_target() {
    pkg_run cmake --build . --target install
}
