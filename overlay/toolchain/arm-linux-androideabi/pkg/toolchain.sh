#!/bin/sh

jagen_pkg_install_target() {
    : ${jagen_toolchain_bin_dir:?}
    : ${jagen_toolchain_dir:?}
    : ${jagen_target_dir:?}
    : ${jagen_target_platform:?}
    : ${jagen_target_toolchain:?}

    rm -fr "$jagen_toolchain_bin_dir"
    mkdir -p "$jagen_toolchain_bin_dir"

    bash "$jagen_toolchain_dir/build/tools/make-standalone-toolchain.sh" \
        --system="linux-x86_64" \
        --platform="$jagen_target_platform" \
        --toolchain="$jagen_target_toolchain" \
        --install-dir="${jagen_target_dir}/${jagen_target_toolchain}"
}
