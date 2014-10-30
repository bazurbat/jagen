#!/bin/sh

p_source="git git@github.com:bazurbat/chicken-eggs.git"
p_source_dir="$ja_src_dir/chicken-eggs"

pkg_install_host() {
    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$ja_build_type" \
        -DCMAKE_C_FLAGS_RELEASE="" \
        -DCMAKE_PREFIX_PATH="$hostdir" \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_cross() {
    use_env tools

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$ja_build_type" \
        -DCMAKE_C_FLAGS_RELEASE="" \
        -DCMAKE_PREFIX_PATH="$toolsdir" \
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
        -DCMAKE_INSTALL_PREFIX="$targetprefix" \
        -DCMAKE_FIND_ROOT_PATH="${targetdir}${targetprefix}" \
        -DCHICKEN_HOST_SYSTEM="mipsel-linux" \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}
