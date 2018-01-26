#!/bin/sh

include_from 'vendor/ast'

jagen_pkg_configure_target() {
    pkg_configure \
        -DHISILICON_ROOT_DIR="${hi_sdk_dir:?}" \
        -DCHICKEN_COMPILER="$jagen_host_dir/bin/chicken" \
        -DCHICKEN_INTERPRETER="$jagen_host_dir/bin/csi" \
        -DCHICKEN_SYSROOT="$jagen_target_dir" \
        -DTARGET_BOARD="$jagen_target_board"
}
