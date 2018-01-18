#!/bin/sh

if [ -z "$jagen_toolchain_prefix" ]; then
    jagen_toolchain_prefix="${jagen_bin_dir:?}/${pkg_build_system}-"
fi

export ARCH="$pkg_build_arch"
export CROSS_COMPILE="$jagen_toolchain_prefix"
