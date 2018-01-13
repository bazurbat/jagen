#!/bin/sh

require toolchain

jagen_pkg_install_target() {
    pkg_install
    toolchain_create_alias \
        "${jagen_bin_dir:?}/arm-hisiv200-linux-" \
        "${jagen_bin_dir:?}/${pkg_build_system:?}-"
}
