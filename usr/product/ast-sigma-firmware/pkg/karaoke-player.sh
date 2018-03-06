#!/bin/sh

include_from 'vendor/ast'

jagen_pkg_configure_target() {
    pkg_configure \
        -DSIGMA_SDK_DIR="${mrua_dir:?}" \
        -DSIGMA_ROOTFS_DIR="${rootfs_source_dir:?}" \
        -DCHICKEN_COMPILER="$jagen_host_dir/bin/chicken" \
        -DCHICKEN_INTERPRETER="$jagen_host_dir/bin/csi" \
        -DCHICKEN_SYSROOT="$jagen_target_dir" \
        -DTARGET_BOARD="$jagen_target_board"
}
