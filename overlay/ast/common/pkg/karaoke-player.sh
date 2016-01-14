#!/bin/sh

jagen_pkg_build_host() {
    pkg_run cmake -G"$jagen_cmake_generator" \
        -DCMAKE_BUILD_TYPE="$jagen_cmake_build_type" \
        -DCMAKE_INSTALL_PREFIX="$jagen_host_dir" \
        -DCMAKE_FIND_ROOT_PATH="$jagen_host_dir" \
        -DTARGET_BOARD="$jagen_target_board" \
        "$pkg_source_dir"

    pkg_run cmake --build . -- $jagen_cmake_build_options
}

jagen_pkg_build_target() {
    local IFS="$(printf '\n\t')"
    local S="$(printf '\t')"
    local A="-G$jagen_cmake_generator"
    A="$A$S-DCMAKE_MODULE_PATH=$jagen_src_dir/cmake-modules"
    A="$A$S-DCMAKE_SYSTEM_NAME=Linux"
    A="$A$S-DCMAKE_BUILD_TYPE=$jagen_cmake_build_type"
    A="$A$S-DCMAKE_INSTALL_PREFIX=${jagen_target_dir}${jagen_target_prefix}"
    A="$A$S-DCHICKEN_COMPILER=$jagen_host_dir/bin/chicken"
    A="$A$S-DCHICKEN_INTERPRETER=$jagen_host_dir/bin/csi"
    A="$A$S-DTARGET_BOARD=$jagen_target_board"

    case $jagen_sdk in
        sigma)
            pkg_run cmake $A \
                -DCMAKE_FIND_ROOT_PATH="${jagen_target_dir}${jagen_target_prefix}" \
                -DSIGMA_SDK_DIR="$jagen_src_dir/sigma-mrua" \
                -DSIGMA_ROOTFS_DIR="$jagen_src_dir/sigma-rootfs" \
                "$pkg_source_dir"
            ;;
        android)
            pkg_run cmake $A \
                -DCMAKE_TOOLCHAIN_FILE="$jagen_src_dir/android-cmake/android.toolchain.cmake" \
                -DANDROID_STANDALONE_TOOLCHAIN="${jagen_target_dir}/${jagen_target_toolchain}" \
                -DHISILICON_ROOT_DIR="$jagen_sdk_dir" \
                -DHISILICON_OUT_DIR="$jagen_android_out_dir" \
                "$pkg_source_dir"
            ;;
        hi-linux)
            pkg_run cmake $A \
                -DHISILICON_ROOT_DIR="$jagen_sdk_dir" \
                "$pkg_source_dir"
            ;;
        *)
            pkg_run cmake $A \
                "$pkg_source_dir"
            ;;
    esac

    pkg_run cmake --build . -- $jagen_cmake_build_options
}

jagen_pkg_install_host() {
    pkg_run cmake --build . --target install
}

jagen_pkg_install_target() {
    pkg_run cmake --build . --target install
}
