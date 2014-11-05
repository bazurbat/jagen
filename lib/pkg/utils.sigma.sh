#!/bin/sh

p_source="git git@bitbucket.org:art-system/sigma-utils.git"
p_source_dir="$ja_src_dir/sigma-utils"
p_build_dir="$p_work_dir/build${p_config:+-$p_config}"

pkg_build_host() {
    use_env tools

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$ja_build_type" \
        -DCMAKE_INSTALL_PREFIX="$tools_dir" \
        -DUSE_LOOPAES=0 \
        ${losetup:+"-DLOSETUP=$losetup"} \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_build_target() {
    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$ja_build_type" \
        -DCMAKE_INSTALL_PREFIX="$sdk_rootfs_root" \
        -DUSE_LOOPAES=1 \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_host() {
    p_run cmake --build . --target install
}

pkg_install_target() {
    p_run cmake --build . --target install
}
