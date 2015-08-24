#!/bin/sh

pkg_build_host() {
    in_flags chicken_next && use_env tools

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$cmake_build_type" \
        -DCMAKE_INSTALL_PREFIX="$host_dir" \
        -DCMAKE_FIND_ROOT_PATH="$host_dir" \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_build_target() {
    case $target_board in
        ast25)
            p_run cmake -G"$cmake_generator" \
                -DCMAKE_BUILD_TYPE="$cmake_build_type" \
                -DCMAKE_SYSTEM_NAME="Linux" \
                -DCMAKE_INSTALL_PREFIX="${target_dir}${target_prefix}" \
                -DCMAKE_FIND_ROOT_PATH="${target_dir}${target_prefix}" \
                -DSIGMA_SDK_DIR="$pkg_src_dir/sigma-mrua" \
                -DSIGMA_SDK_VERSION="$pkg_sdk_version" \
                -DSIGMA_ROOTFS_DIR="$pkg_src_dir/sigma-rootfs" \
                -DCHICKEN_COMPILER="$host_dir/bin/chicken" \
                -DCHICKEN_INTERPRETER="$host_dir/bin/csi" \
                -DCHICKEN_DEPENDS="$host_dir/bin/chicken-depends" \
                -DTARGET_BOARD="$target_board" \
                "$p_source_dir"
            ;;
        ast50|ast100)
            p_run cmake -G"$cmake_generator" \
                -DCMAKE_BUILD_TYPE="$cmake_build_type" \
                -DCMAKE_SYSTEM_NAME="Linux" \
                -DCMAKE_INSTALL_PREFIX="${target_dir}${target_prefix}" \
                -DCMAKE_FIND_ROOT_PATH="${target_dir}${target_prefix}" \
                -DSIGMA_SDK_DIR="$pkg_src_dir/sigma-mrua" \
                -DSIGMA_SDK_VERSION="$pkg_sdk_version" \
                -DSIGMA_ROOTFS_DIR="$pkg_src_dir/sigma-rootfs" \
                -DCHICKEN_COMPILER="$host_dir/bin/chicken" \
                -DCHICKEN_INTERPRETER="$host_dir/bin/csi" \
                -DCHICKEN_DEPENDS="$host_dir/bin/chicken-depends" \
                -DTARGET_BOARD="$target_board" \
                "$p_source_dir"
            ;;
        *)
            p_run cmake -G"$cmake_generator" \
                -DCMAKE_TOOLCHAIN_FILE="$pkg_src_dir/android-cmake/android.toolchain.cmake" \
                -DANDROID_STANDALONE_TOOLCHAIN="$android_toolchain_dir" \
                -DANDROID_GOLD_LINKER=NO \
                -DCMAKE_BUILD_TYPE="$cmake_build_type" \
                -DCMAKE_SYSTEM_NAME="Linux" \
                -DCMAKE_INSTALL_PREFIX="${target_dir}${target_prefix}" \
                -DCMAKE_FIND_ROOT_PATH="${target_dir}${target_prefix}" \
                -DHISILICON_ROOT_DIR="$sdk_dir" \
                -DHISILICON_OUT_DIR="$sdk_out_dir" \
                -DTARGET_BOARD="$target_board" \
                "$p_source_dir"
            ;;
    esac

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_host() {
    p_run cmake --build . --target install
}

pkg_install_target() {
    p_run cmake --build . --target install
}
