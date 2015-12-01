#!/bin/sh

jagen_pkg_install() {
    : ${jagen_toolchain_dir:?}

    rm -fr "$toolchain_bin_dir"
    mkdir -p "$toolchain_bin_dir"

    bash "$jagen_toolchain_dir/build/tools/make-standalone-toolchain.sh" \
        --system="linux-x86_64" \
        --platform="$target_platform" \
        --toolchain="$target_toolchain" \
        --install-dir="${jagen_target_dir}/${target_toolchain}"
}
