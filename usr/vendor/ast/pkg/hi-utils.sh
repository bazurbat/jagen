#!/bin/sh

jagen_pkg_configure_target() {
    pkg_configure \
        "-DHISILICON_ROOT_DIR=${hi_sdk_dir:?}"
}
