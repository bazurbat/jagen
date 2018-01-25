#!/bin/sh

include_from 'vendor/ast'

jagen_pkg_configure_target() {
    pkg_configure \
        $(pkg_using_hisilicon_sdk) \
        $(pkg_using_host_chicken) \
        $(pkg_using_target_board)
}
