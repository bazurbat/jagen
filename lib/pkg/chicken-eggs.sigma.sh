#!/bin/sh

p_source="git https://github.com/bazurbat/chicken-eggs.git"
p_source_dir="$pkg_src_dir/chicken-eggs"
p_source_branch="master"
p_build_dir="$p_work_dir/build${p_config:+-$p_config}"

if p_flags chicken_next; then
    p_source_branch="next"
fi

pkg_install_host() {
    p_flags chicken_next && return 0

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$cmake_build_type" \
        -DCMAKE_C_FLAGS_RELEASE="" \
        -DCMAKE_PREFIX_PATH="$host_dir" \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_cross() {
    use_env tools

    if p_flags chicken_next; then
        p_run cmake -G"$cmake_generator" \
            -DCMAKE_BUILD_TYPE="$cmake_build_type" \
            -DCMAKE_C_FLAGS_RELEASE="" \
            -DCMAKE_PREFIX_PATH="$tools_dir" \
            "$p_source_dir"
    else
        p_run cmake -G"$cmake_generator" \
            -DCMAKE_BUILD_TYPE="$cmake_build_type" \
            -DCMAKE_C_FLAGS_RELEASE="" \
            -DCMAKE_PREFIX_PATH="$tools_dir" \
            -DCHICKEN_SYSTEM="$target_system" \
            "$p_source_dir"
    fi

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_target() {
    use_env host

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$cmake_build_type" \
        -DCMAKE_C_FLAGS_RELEASE="" \
        -DCMAKE_SYSTEM_NAME="Linux" \
        -DCMAKE_INSTALL_PREFIX="$target_prefix" \
        -DCMAKE_FIND_ROOT_PATH="${target_dir}${target_prefix}" \
        -DCHICKEN_HOST_SYSTEM="$target_system" \
        -DCHICKEN_EXECUTABLE="$host_dir/bin/chicken" \
        -DCHICKEN_CSI_EXECUTABLE="$host_dir/bin/csi" \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}
