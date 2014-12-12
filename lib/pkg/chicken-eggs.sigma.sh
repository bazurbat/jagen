#!/bin/sh

p_source="git https://github.com/bazurbat/chicken-eggs.git"
p_source_dir="$pkg_src_dir/chicken-eggs"
p_source_branch="master"
p_build_dir="$p_work_dir/build${p_config:+-$p_config}"

if p_flags chicken_next; then
    p_source_branch="next"
fi

# FIXME: add proper support for other build types to chicken
case $cmake_build_type in
    Debug|Release) ;;
    *) cmake_build_type=Release ;;
esac

pkg_install_host() {
    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$cmake_build_type" \
        -DCMAKE_INSTALL_PREFIX="$host_dir" \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_target() {
    use_env host
    export DESTDIR="$target_dir"

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$cmake_build_type" \
        -DCMAKE_SYSTEM_NAME="Linux" \
        -DCMAKE_INSTALL_PREFIX="$target_prefix" \
        -DCMAKE_FIND_ROOT_PATH="$target_dir$target_prefix" \
        -DCHICKEN_COMPILER="$host_dir/bin/chicken" \
        -DCHICKEN_INTERPRETER="$host_dir/bin/csi" \
        -DCHICKEN_DEPENDS="$host_dir/bin/chicken-depends" \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}
