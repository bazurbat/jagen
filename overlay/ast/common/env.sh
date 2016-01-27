#!/bin/sh

jagen_cmake_module_path="$jagen_src_dir/cmake-modules"

pkg_using_android_toolchain() {
    local S="$jagen_FS" A=
    A="$A$S-DCMAKE_TOOLCHAIN_FILE=$jagen_src_dir/android-cmake/android.toolchain.cmake"
    A="$A$S-DANDROID_STANDALONE_TOOLCHAIN=${jagen_target_dir}/${jagen_target_toolchain}"
    printf '%s' "$A"
}

pkg_using_hisilicon_sdk() {
    local S="$jagen_FS" A=
    A="$A$S-DHISILICON_ROOT_DIR=$jagen_sdk_dir"
    A="$A$S-DHISILICON_OUT_DIR=$jagen_sdk_staging_dir"
    printf '%s' "$A"
}
