#!/bin/sh

jagen_pkg_install_target() {
    : ${jagen_project_dir:?}
    : ${jagen_toolchain_dir:?}
    : ${jagen_target_platform:?}
    : ${jagen_target_toolchain:?}

    # TODO: look at the make-standalone-toolchain script to see if we need to
    # delete install-dir beforehand

    bash "$jagen_toolchain_dir/build/tools/make-standalone-toolchain.sh" \
        --system="linux-x86_64" \
        --platform="$jagen_target_platform" \
        --toolchain="$jagen_target_toolchain" \
        --install-dir="$jagen_project_dir/$jagen_target_toolchain"
}
