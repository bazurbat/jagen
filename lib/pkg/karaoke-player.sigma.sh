#!/bin/sh

psourcedir="$ja_srcdir/karaoke-player"
pbuilddir="$pkg_builddir/${pname}${pconfig:+-$pconfig}"

pkg_unpack() {
    :
}

pkg_build_host() {
    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$ja_buildtype" \
        -DCMAKE_PREFIX_PATH="$hostdir" \
        -DCMAKE_INSTALL_PREFIX="$hostdir" \
        "$psourcedir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_build_target() {
    use_env tools

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$ja_buildtype" \
        -DCMAKE_SYSTEM_NAME="Linux" \
        -DCMAKE_INSTALL_PREFIX="${targetdir}${targetprefix}" \
        -DCMAKE_FIND_ROOT_PATH="${targetdir}${targetprefix}" \
        -DSIGMA_ROOT_DIR="$ja_srcdir" \
        -DCHICKEN_HOST_SYSTEM="mipsel-linux" \
        -DCHICKEN_BUILD_IMPORTS=NO \
        -DLIBUV_ROOT_DIR="$sdk_rootfs_prefix" \
        "$psourcedir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_host() {
    p_run cmake --build . --target install
}

pkg_install_target() {
    p_run cmake --build . --target install
}
