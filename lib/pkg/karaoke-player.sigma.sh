#!/bin/sh

p_source="hg ssh://hg@bitbucket.org/art-system/karaoke-player"
p_source_dir="$pkg_src_dir/karaoke-player"
p_build_dir="$p_work_dir/build${p_config:+-$p_config}"

pkg_build_host() {
    p_flags chicken_next && use_env tools

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$cmake_build_type" \
        -DCMAKE_PREFIX_PATH="$host_dir" \
        -DCMAKE_INSTALL_PREFIX="$host_dir" \
        -DCHICKEN_DEPENDS="$tools_dir/bin/chicken-depends" \
        "$p_source_dir"

    if p_flags libuv_next; then
        p_run cmake -DLIBUV_NEW=1 .
    fi

    p_run cmake --build . -- $cmake_build_options
}

pkg_prepare_target() {
    p_run cd "$pkg_src_dir"
    p_run ln -sfT sigma-rootfs rootfs
    p_run ln -sfT sigma-mrua mrua
}

pkg_build_target() {
    use_env tools

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$cmake_build_type" \
        -DCMAKE_SYSTEM_NAME="Linux" \
        -DCMAKE_INSTALL_PREFIX="${target_dir}${target_prefix}" \
        -DCMAKE_FIND_ROOT_PATH="${target_dir}${target_prefix}" \
        -DSIGMA_ROOT_DIR="$pkg_src_dir" \
        -DCHICKEN_HOST_SYSTEM="mipsel-linux" \
        -DCHICKEN_BUILD_IMPORTS=NO \
        -DCHICKEN_COMPILER="$tools_dir/bin/chicken" \
        -DCHICKEN_INTERPRETER="$tools_dir/bin/csi" \
        -DCHICKEN_DEPENDS="$tools_dir/bin/chicken-depends" \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_host() {
    p_run cmake --build . --target install
}

pkg_install_target() {
    p_run cmake --build . --target install
}
