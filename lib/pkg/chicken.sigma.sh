#!/bin/sh

p_source="git git@github.com:bazurbat/chicken-scheme.git"
p_source_dir="$ja_src_dir/chicken-scheme"
p_source_branch="cmake"

pkg_build_host() {
    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$ja_build_type" \
        -DCMAKE_INSTALL_PREFIX="$hostdir" \
        -DCMAKE_C_FLAGS_RELEASE="" \
        -DCHICKEN_API_VERSION=6 \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_build_cross() {
    use_env host tools

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$ja_build_type" \
        -DCMAKE_C_FLAGS_RELEASE="" \
        -DCMAKE_PREFIX_PATH="$hostdir" \
        -DCMAKE_INSTALL_PREFIX="$toolsdir" \
        -DCHICKEN_TARGET_NAME="chicken" \
        -DCHICKEN_TARGET_SYSTEM="mipsel-linux" \
        -DCHICKEN_TARGET_ROOT_DIR="$targetdir" \
        -DCHICKEN_TARGET_RUN_PREFIX="$targetprefix" \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_build_target() {
    use_env tools

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$ja_build_type" \
        -DCMAKE_C_FLAGS_RELEASE="" \
        -DCMAKE_SYSTEM_NAME="Linux" \
        -DCMAKE_SYSTEM_PROCESSOR="mips32" \
        -DCMAKE_PREFIX_PATH="$hostdir" \
        -DCMAKE_INSTALL_PREFIX="$targetprefix" \
        -DCHICKEN_HOST_SYSTEM="mipsel-linux" \
        -DCHICKEN_TARGET_FEATURES="-no-feature x86 -feature mips" \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_host() {
    p_run cmake --build . --target install -- $cmake_build_options
}

pkg_install_cross() {
    p_run cmake --build . --target install -- $cmake_build_options
}

pkg_install_target() {
    DESTDIR="$targetdir" p_run cmake --build . \
        --target install -- $cmake_build_options
}
