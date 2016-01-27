#!/bin/sh

jagen_pkg_build_host() {
    # Ignore already installed CHICKEN in the pacakge prefix for clean rebuild.
    # Newer CMake has more convenient CMAKE_FIND_NO_INSTALL_PREFIX for the same
    # purpose, but we are stuck with 2.8.12 for now.

    default_build -DCMAKE_SYSTEM_IGNORE_PATH="$pkg_prefix"
}

jagen_pkg_build_target() {
    local IFS="$jagen_IFS" S="$jagen_FS" A=

    A="-DCHICKEN_COMPILER=$jagen_host_dir/bin/chicken"
    A="$A$S-DCHICKEN_INTERPRETER=$jagen_host_dir/bin/csi"

    case $jagen_sdk in
        sigma)
            default_build \
                -DCMAKE_SYSTEM_PROCESSOR="mips32"
            ;;
        android)
            pkg_run cmake -G"$jagen_cmake_generator" \
                -DCMAKE_TOOLCHAIN_FILE="$jagen_src_dir/android-cmake/android.toolchain.cmake" \
                -DANDROID_STANDALONE_TOOLCHAIN="${jagen_target_dir}/${jagen_target_toolchain}" \
                -DANDROID_GOLD_LINKER=NO \
                -DCMAKE_BUILD_TYPE="$jagen_cmake_build_type" \
                -DCMAKE_SYSTEM_NAME="Linux" \
                -DCMAKE_INSTALL_PREFIX="$jagen_target_prefix" \
                "$pkg_source_dir"
            pkg_run cmake --build . -- $jagen_cmake_build_options
            ;;
        *)
            default_build $A
            ;;
    esac
}
