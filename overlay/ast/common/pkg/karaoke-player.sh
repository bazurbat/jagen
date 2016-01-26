#!/bin/sh

jagen_pkg_build_host() {
    default_build \
        -DCMAKE_FIND_ROOT_PATH="$jagen_host_dir" \
        -DTARGET_BOARD="$jagen_target_board"
}

jagen_pkg_build_target() {
    local IFS="$jagen_IFS" S="$jagen_FS" A=

    A="$A$S-DCHICKEN_COMPILER=$jagen_host_dir/bin/chicken"
    A="$A$S-DCHICKEN_INTERPRETER=$jagen_host_dir/bin/csi"
    A="$A$S-DTARGET_BOARD=$jagen_target_board"

    case $jagen_sdk in
        sigma)
            default_build $A \
                -DCMAKE_FIND_ROOT_PATH="${jagen_target_dir}${jagen_target_prefix}" \
                -DSIGMA_SDK_DIR="$jagen_src_dir/sigma-mrua" \
                -DSIGMA_ROOTFS_DIR="$jagen_src_dir/sigma-rootfs"
            ;;
        android)
            default_build $A \
                -DCMAKE_TOOLCHAIN_FILE="$jagen_src_dir/android-cmake/android.toolchain.cmake" \
                -DANDROID_STANDALONE_TOOLCHAIN="${jagen_target_dir}/${jagen_target_toolchain}" \
                -DHISILICON_ROOT_DIR="$jagen_sdk_dir" \
                -DHISILICON_OUT_DIR="$jagen_sdk_staging_dir"
            ;;
        *)
            default_build $A
            ;;
    esac
}
