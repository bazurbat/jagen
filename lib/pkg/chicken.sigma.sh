#!/bin/sh

p_source="git https://github.com/bazurbat/chicken-scheme.git"
p_source_dir="$pkg_src_dir/chicken-scheme"
p_source_branch="cmake"
p_build_dir="$p_work_dir/build${p_config:+-$p_config}"

if in_flags chicken_next; then
    p_source_branch="next"
fi

# FIXME: add proper support for other build types to chicken
case $cmake_build_type in
    Debug|Release) ;;
    *) cmake_build_type=Release ;;
esac

pkg_build_host() {
    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$cmake_build_type" \
        -DCMAKE_INSTALL_PREFIX="$host_dir" \
        -DCMAKE_FIND_NO_INSTALL_PREFIX=TRUE \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_build_target() {
    use_env host

    p_run cmake -G"$cmake_generator" \
        -DCMAKE_BUILD_TYPE="$cmake_build_type" \
        -DCMAKE_SYSTEM_NAME="Linux" \
        -DCMAKE_SYSTEM_PROCESSOR="mips32" \
        -DCMAKE_INSTALL_PREFIX="$target_prefix" \
        "$p_source_dir"

    p_run cmake --build . -- $cmake_build_options
}

pkg_install_host() {
    p_run cmake --build . --target install -- $cmake_build_options
}

pkg_install_target() {
    # supplying this directly on command line fails on Ubuntu
    export DESTDIR="$target_dir"
    p_run cmake --build . --target install -- $cmake_build_options
}
