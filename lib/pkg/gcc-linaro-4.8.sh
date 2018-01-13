#!/bin/sh

require toolchain

jagen_pkg_install_target() {
    toolchain_generate_wrappers    \
        "${jagen_bin_dir:?}"       \
        "${pkg_source_dir:?}/bin"  \
        "${pkg_build_target_system:?}"
}
