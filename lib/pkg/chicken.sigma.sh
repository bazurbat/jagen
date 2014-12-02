#!/bin/sh

p_source="git https://github.com/bazurbat/chicken-scheme.git"
p_source_dir="$pkg_src_dir/chicken-scheme"
p_source_branch="cmake"
p_build_dir="$p_work_dir/build${p_config:+-$p_config}"

if p_flags chicken_next; then
    p_source_branch="next"
fi

pkg_build_host() {
    p_flags chicken_next && return 0

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$cmake_build_type" \
        -DCMAKE_INSTALL_PREFIX="$host_dir" \
        -DCMAKE_C_FLAGS_RELEASE="" \
        -DCHICKEN_API_VERSION=6 \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_build_cross() {
    use_env host tools

    if p_flags chicken_next; then
        p_run cmake -G"$cmake_generator" \
            -DCMAKE_BUILD_TYPE="$cmake_build_type" \
            -DCMAKE_INSTALL_PREFIX="$tools_dir" \
            -DCMAKE_C_FLAGS_RELEASE="" \
            -DCHICKEN_API_VERSION=6 \
            "$p_source_dir"
    else
        p_run cmake -G"$cmake_generator" \
            -DCMAKE_BUILD_TYPE="$cmake_build_type" \
            -DCMAKE_C_FLAGS_RELEASE="" \
            -DCMAKE_PREFIX_PATH="$host_dir" \
            -DCMAKE_INSTALL_PREFIX="$tools_dir" \
            -DCHICKEN_TARGET_NAME="chicken" \
            -DCHICKEN_TARGET_SYSTEM="mipsel-linux" \
            -DCHICKEN_TARGET_ROOT_DIR="$target_dir" \
            -DCHICKEN_TARGET_RUN_PREFIX="$target_prefix" \
            "$p_source_dir"
    fi

    p_run cmake --build . -- $cmake_build_options
}

pkg_build_target() {
    use_env host

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$cmake_build_type" \
        -DCMAKE_C_FLAGS_RELEASE="" \
        -DCMAKE_SYSTEM_NAME="Linux" \
        -DCMAKE_SYSTEM_PROCESSOR="mips32" \
        -DCMAKE_PREFIX_PATH="$host_dir" \
        -DCMAKE_INSTALL_PREFIX="$target_prefix" \
        -DCHICKEN_HOST_SYSTEM="mipsel-linux" \
        -DCHICKEN_TARGET_FEATURES="-no-feature x86 -feature mips" \
		-DCHICKEN_EXECUTABLE="$host_dir/bin/chicken" \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_host() {
    p_flags chicken_next && return 0

    p_run cmake --build . --target install -- $cmake_build_options
}

pkg_install_cross() {
    use_env host tools
    p_run cmake --build . --target install -- $cmake_build_options
}

pkg_install_target() {
    use_env tools
    # supplying this directly on command line fails on Ubuntu
    export DESTDIR="$target_dir"
    p_run cmake --build . --target install -- $cmake_build_options
}
