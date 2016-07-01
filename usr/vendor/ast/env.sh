#!/bin/sh

jagen_private_dir="${jagen_src_dir:?}/files"

jagen_cmake_module_path="$jagen_src_dir/cmake-modules"

jagen_dist_patches_dir="$jagen_src_dir/patches"

pkg_using_android_toolchain() {
    local S="$jagen_FS" A=
    A="$A$S-DCMAKE_TOOLCHAIN_FILE=${jagen_src_dir:?}/android-cmake/android.toolchain.cmake"
    A="$A$S-DANDROID_STANDALONE_TOOLCHAIN=${jagen_target_dir:?}/${jagen_target_toolchain:?}"
    # sometimes it is empty, this is workaround
    A="$A$S-DCMAKE_CXX_COMPILER=\${CMAKE_C_COMPILER}"
    printf '%s' "$A"
}

pkg_using_hisilicon_sdk() {
    local S="$jagen_FS" A=
    A="$A$S-DHISILICON_ROOT_DIR=$jagen_sdk_dir"
    case $jagen_sdk in
        android)
            A="$A$S-DHISILICON_OBJ_DIR=$jagen_sdk_staging_dir/obj"
            ;;
    esac
    printf '%s' "$A"
}
