#!/bin/sh

# FIXME: add proper support for other build types to chicken
case $jagen_cmake_build_type in
    Debug|Release) ;;
    *) jagen_cmake_build_type=Release ;;
esac

jagen_pkg_build_host() {
    pkg_run cmake -G"$jagen_cmake_generator" \
        -DCMAKE_BUILD_TYPE="$jagen_cmake_build_type" \
        -DCMAKE_INSTALL_PREFIX="$jagen_host_dir" \
        -DCMAKE_FIND_NO_INSTALL_PREFIX=TRUE \
        "$pkg_source_dir"

    pkg_run cmake --build . -- $jagen_cmake_build_options
}

jagen_pkg_build_target() {
    case $target_board in
        ast25|ast50|ast100)
            pkg_run cmake -G"$jagen_cmake_generator" \
                -DCMAKE_BUILD_TYPE="$jagen_cmake_build_type" \
                -DCMAKE_SYSTEM_NAME="Linux" \
                -DCMAKE_SYSTEM_PROCESSOR="mips32" \
                -DCMAKE_INSTALL_PREFIX="$jagen_target_prefix" \
                "$pkg_source_dir"
            ;;
        *)
            pkg_run cmake -G"$jagen_cmake_generator" \
                -DCMAKE_TOOLCHAIN_FILE="$jagen_src_dir/android-cmake/android.toolchain.cmake" \
                -DANDROID_STANDALONE_TOOLCHAIN="${jagen_target_dir}/${target_toolchain}" \
                -DANDROID_GOLD_LINKER=NO \
                -DCMAKE_BUILD_TYPE="$jagen_cmake_build_type" \
                -DCMAKE_SYSTEM_NAME="Linux" \
                -DCMAKE_INSTALL_PREFIX="$jagen_target_prefix" \
                "$pkg_source_dir"
            ;;
    esac

    pkg_run cmake --build . -- $jagen_cmake_build_options
}

jagen_pkg_install_host() {
    pkg_run cmake --build . --target install -- $jagen_cmake_build_options
}

jagen_pkg_install_target() {
    # supplying this directly on command line fails on Ubuntu
    export DESTDIR="$jagen_target_dir"
    pkg_run cmake --build . --target install -- $jagen_cmake_build_options
}
