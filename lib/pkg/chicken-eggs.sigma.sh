#!/bin/sh

psourcedir="$ja_srcdir/chicken-eggs"
pbuilddir="${pworkdir}/${pname}${pconfig:+-$pconfig}"

use_env cmake

pkg_unpack() {
    :
}

pkg_install_host() {
    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$ja_buildtype" \
        -DCMAKE_C_FLAGS_RELEASE="" \
        -DCMAKE_PREFIX_PATH="$hostdir" \
        "$psourcedir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_cross() {
    use_env tools

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$ja_buildtype" \
        -DCMAKE_C_FLAGS_RELEASE="" \
        -DCMAKE_PREFIX_PATH="$toolsdir" \
        -DCHICKEN_SYSTEM="mipsel-linux" \
        "$psourcedir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_target() {
    use_env tools

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$ja_buildtype" \
        -DCMAKE_C_FLAGS_RELEASE="" \
        -DCMAKE_SYSTEM_NAME="Linux" \
        -DCMAKE_INSTALL_PREFIX="$targetprefix" \
        -DCMAKE_FIND_ROOT_PATH="${targetdir}${targetprefix}" \
        -DCHICKEN_HOST_SYSTEM="mipsel-linux" \
        "$psourcedir"

    p_run cmake --build . -- $cmake_build_options
}
