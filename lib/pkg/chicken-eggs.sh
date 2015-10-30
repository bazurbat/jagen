#!/bin/sh

# FIXME: add proper support for other build types to chicken
case $jagen_cmake_build_type in
    Debug|Release) ;;
    *) jagen_cmake_build_type=Release ;;
esac

delete_install_targets() {
    p_run find "$p_build_dir" -name "*-install" -delete
}

pkg_install_host() {
    delete_install_targets

    p_run cmake -G"$jagen_cmake_generator" \
        -DCMAKE_BUILD_TYPE="$jagen_cmake_build_type" \
        -DCMAKE_INSTALL_PREFIX="$jagen_host_dir" \
        "$p_source_dir"

    p_run cmake --build . -- $jagen_cmake_build_options
}

pkg_install_target() {
    delete_install_targets

    case $target_board in
        ast25|ast50|ast100)
            p_run cmake -G"$jagen_cmake_generator" \
                -DCMAKE_BUILD_TYPE="$jagen_cmake_build_type" \
                -DCMAKE_SYSTEM_NAME="Linux" \
                -DCMAKE_FIND_ROOT_PATH="$jagen_target_dir$jagen_target_prefix" \
                -DCHICKEN_COMPILER="$jagen_host_dir/bin/chicken" \
                -DCHICKEN_INTERPRETER="$jagen_host_dir/bin/csi" \
                -DCHICKEN_DEPENDS="$jagen_host_dir/bin/chicken-depends" \
                "$p_source_dir"
            ;;
        *)
            p_run cmake -G"$jagen_cmake_generator" \
                -DCMAKE_TOOLCHAIN_FILE="$jagen_src_dir/android-cmake/android.toolchain.cmake" \
                -DANDROID_STANDALONE_TOOLCHAIN="${jagen_target_dir}/${target_toolchain}" \
                -DCMAKE_BUILD_TYPE="$jagen_cmake_build_type" \
                -DCMAKE_SYSTEM_NAME="Linux" \
                -DCMAKE_INSTALL_PREFIX="${jagen_target_dir}${jagen_target_prefix}" \
                -DCHICKEN_COMPILER="$jagen_host_dir/bin/chicken" \
                -DCHICKEN_INTERPRETER="$jagen_host_dir/bin/csi" \
                -DCHICKEN_DEPENDS="$jagen_host_dir/bin/chicken-depends" \
                "$p_source_dir"
            ;;
    esac

    p_run cmake --build . -- $jagen_cmake_build_options
}
