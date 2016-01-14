#!/bin/sh

if [ -z "$jagen_target_toolchain_dir" ]; then
    : ${jagen_toolchain_dir:?}
    jagen_target_toolchain_dir="$jagen_toolchain_dir/bin"
fi

if [ -d "$jagen_target_toolchain_dir" ]; then
    add_PATH "$jagen_target_toolchain_dir"
fi

if [ -z "$jagen_toolchain_prefix" ]; then
    : ${jagen_target_system:?}
    jagen_toolchain_prefix="${jagen_target_toolchain_dir}/${jagen_target_system}-"
fi

export CROSS_COMPILE="$jagen_toolchain_prefix"
