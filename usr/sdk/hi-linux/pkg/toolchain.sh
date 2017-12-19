#!/bin/sh

require toolchain

jagen_pkg_install_target() {
    toolchain_create_alias \
        "${jagen_bin_dir:?}/arm-hisiv200-linux-" \
        "${jagen_bin_dir:?}/${jagen_target_system:?}-"
}
