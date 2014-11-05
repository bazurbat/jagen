#!/bin/sh

p_source="git git@github.com:bazurbat/chicken-eggs.git"
p_source_dir="$ja_src_dir/chicken-eggs"
p_build_dir="$p_work_dir/build${p_config:+-$p_config}"

pkg_install_host() {
    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$ja_build_type" \
        -DCMAKE_C_FLAGS_RELEASE="" \
        -DCMAKE_PREFIX_PATH="$host_dir" \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_cross() {
    use_env tools

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$ja_build_type" \
        -DCMAKE_C_FLAGS_RELEASE="" \
        -DCMAKE_PREFIX_PATH="$tools_dir" \
        -DCHICKEN_SYSTEM="mipsel-linux" \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_target() {
    use_env tools

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$ja_build_type" \
        -DCMAKE_C_FLAGS_RELEASE="" \
        -DCMAKE_SYSTEM_NAME="Linux" \
        -DCMAKE_INSTALL_PREFIX="$target_prefix" \
        -DCMAKE_FIND_ROOT_PATH="${target_dir}${target_prefix}" \
        -DCHICKEN_HOST_SYSTEM="mipsel-linux" \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}
