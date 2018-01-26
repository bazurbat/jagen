#!/bin/sh

include_from 'vendor/ast'

jagen_pkg_configure_target() {
    pkg_configure \
        -DSIGMA_SDK_DIR="${mrua_dir:?}" \
        -DSIGMA_ROOTFS_DIR="${rootfs_dir:?}" \
        $(pkg_using_host_chicken) \
        "-DTARGET_BOARD=$jagen_target_board"
}
