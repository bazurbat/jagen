#!/bin/sh

p_source="hg ssh://hg@bitbucket.org/art-system/karaoke-player"
p_source_dir="$ja_src_dir/karaoke-player"
p_build_dir="$p_work_dir${p_config:+/$p_config}"

pkg_build_host() {
    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$ja_build_type" \
        -DCMAKE_PREFIX_PATH="$hostdir" \
        -DCMAKE_INSTALL_PREFIX="$hostdir" \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_build_target() {
    use_env tools

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$ja_build_type" \
        -DCMAKE_SYSTEM_NAME="Linux" \
        -DCMAKE_INSTALL_PREFIX="${targetdir}${targetprefix}" \
        -DCMAKE_FIND_ROOT_PATH="${targetdir}${targetprefix}" \
        -DSIGMA_ROOT_DIR="$ja_src_dir" \
        -DCHICKEN_HOST_SYSTEM="mipsel-linux" \
        -DCHICKEN_BUILD_IMPORTS=NO \
        -DLIBUV_ROOT_DIR="$sdk_rootfs_prefix" \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_host() {
    p_run cmake --build . --target install
}

pkg_install_target() {
    p_run cmake --build . --target install
}
