#!/bin/sh

p_source_dir="$pkg_src_dir/karaoke-player"
p_build_dir="$p_work_dir/build${p_config:+-$p_config}"

pkg_build_host() {
    in_flags chicken_next && use_env tools

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$cmake_build_type" \
        -DCMAKE_INSTALL_PREFIX="$host_dir" \
        -DCMAKE_FIND_ROOT_PATH="$host_dir" \
        "$p_source_dir"

    if in_flags libuv_next; then
        p_run cmake -DLIBUV_NEW=1 .
    fi

    p_run cmake --build . -- $cmake_build_options
}

pkg_build_target() {
    use_env host

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
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_host() {
    p_run cmake --build . --target install
}

pkg_install_target() {
    p_run cmake --build . --target install
}
