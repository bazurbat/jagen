#!/bin/sh

jagen_pkg_unpack() {
    toolchain_unpack "$pkg_name" "$pkg_source_dir"
}

jagen_pkg_install_target() {
    toolchain_generate_wrappers    \
        "${jagen_bin_dir:?}"       \
        "${pkg_source_dir:?}/bin"  \
        "${jagen_target_system:?}"
}
