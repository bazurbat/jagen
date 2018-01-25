#!/bin/sh

include_from 'vendor/ast'

jagen_pkg_configure_target() {
    pkg_configure \
        "-DHISILICON_ROOT_DIR=${hi_sdk_dir:?}" \
        $(pkg_using_host_chicken) \
        $(pkg_using_target_board)
}