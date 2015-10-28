#!/bin/sh

# FIXME: add proper support for other build types to chicken
case $cmake_build_type in
    Debug|Release) ;;
    *) cmake_build_type=Release ;;
esac

pkg_build_host() {
    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$cmake_build_type" \
        -DCMAKE_INSTALL_PREFIX="$host_dir" \
        -DCMAKE_FIND_NO_INSTALL_PREFIX=TRUE \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_build_target() {
    case $target_board in
        ast25|ast50|ast100)
            p_run cmake -G"$cmake_generator" \
                -DCMAKE_BUILD_TYPE="$cmake_build_type" \
                -DCMAKE_SYSTEM_NAME="Linux" \
                -DCMAKE_SYSTEM_PROCESSOR="mips32" \
                -DCMAKE_INSTALL_PREFIX="$target_prefix" \
                "$p_source_dir"
            ;;
        *)
            p_run cmake -G"$cmake_generator" \
                -DCMAKE_TOOLCHAIN_FILE="$pkg_src_dir/android-cmake/android.toolchain.cmake" \
                -DANDROID_STANDALONE_TOOLCHAIN="${target_dir}/${target_toolchain}" \
                -DANDROID_GOLD_LINKER=NO \
                -DCMAKE_BUILD_TYPE="$cmake_build_type" \
                -DCMAKE_SYSTEM_NAME="Linux" \
                -DCMAKE_INSTALL_PREFIX="$target_prefix" \
                "$p_source_dir"
            ;;
    esac

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_host() {
    p_run cmake --build . --target install -- $cmake_build_options
}

pkg_install_target() {
    # supplying this directly on command line fails on Ubuntu
    export DESTDIR="$target_dir"
    p_run cmake --build . --target install -- $cmake_build_options
}
