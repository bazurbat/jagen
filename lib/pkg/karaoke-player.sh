#!/bin/sh

pkg_build_host() {
    pkg_run cmake -G"$jagen_cmake_generator" \
        -DCMAKE_BUILD_TYPE="$jagen_cmake_build_type" \
        -DCMAKE_INSTALL_PREFIX="$jagen_host_dir" \
        -DCMAKE_FIND_ROOT_PATH="$jagen_host_dir" \
        -DTARGET_BOARD="$target_board" \
        "$p_source_dir"

    pkg_run cmake --build . -- $jagen_cmake_build_options
}

pkg_build_target() {
    case $target_board in
        ast25|ast50|ast100)
            pkg_run cmake -G"$jagen_cmake_generator" \
                -DCMAKE_BUILD_TYPE="$jagen_cmake_build_type" \
                -DCMAKE_SYSTEM_NAME="Linux" \
                -DCMAKE_INSTALL_PREFIX="${jagen_target_dir}${jagen_target_prefix}" \
                -DCMAKE_FIND_ROOT_PATH="${jagen_target_dir}${jagen_target_prefix}" \
                -DSIGMA_SDK_DIR="$jagen_src_dir/sigma-mrua" \
                -DSIGMA_ROOTFS_DIR="$jagen_src_dir/sigma-rootfs" \
                -DCHICKEN_COMPILER="$jagen_host_dir/bin/chicken" \
                -DCHICKEN_INTERPRETER="$jagen_host_dir/bin/csi" \
                -DCHICKEN_DEPENDS="$jagen_host_dir/bin/chicken-depends" \
                -DTARGET_BOARD="$target_board" \
                "$p_source_dir"
            ;;
        *)
            pkg_run cmake -G"$jagen_cmake_generator" \
                -DCMAKE_TOOLCHAIN_FILE="$jagen_src_dir/android-cmake/android.toolchain.cmake" \
                -DANDROID_STANDALONE_TOOLCHAIN="${jagen_target_dir}/${target_toolchain}" \
                -DCMAKE_BUILD_TYPE="$jagen_cmake_build_type" \
                -DCMAKE_SYSTEM_NAME="Linux" \
                -DCMAKE_INSTALL_PREFIX="${jagen_target_dir}${jagen_target_prefix}" \
                -DCHICKEN_COMPILER="$jagen_host_dir/bin/chicken" \
                -DCHICKEN_INTERPRETER="$jagen_host_dir/bin/csi" \
                -DHISILICON_ROOT_DIR="$jagen_sdk_dir" \
                -DHISILICON_OUT_DIR="$sdk_out_dir" \
                -DTARGET_BOARD="$target_board" \
                "$p_source_dir"
            ;;
    esac

    pkg_run cmake --build . -- $jagen_cmake_build_options
}

pkg_install_host() {
    pkg_run cmake --build . --target install
}

pkg_install_target() {
    pkg_run cmake --build . --target install
}
