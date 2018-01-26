#!/bin/sh

delete_install_targets() {
    pkg_run find "$pkg_build_dir" -name "*-install" -delete
}

jagen_pkg_configure_host() {
    delete_install_targets

    pkg_configure
}

jagen_pkg_configure_target() {
    delete_install_targets

    pkg_configure \
        -DCHICKEN_COMPILER="$jagen_host_dir/bin/chicken" \
        -DCHICKEN_INTERPRETER="$jagen_host_dir/bin/csi" \
        -DCHICKEN_SYSROOT="$jagen_target_dir"
}

jagen_pkg_install() {
    :
}
