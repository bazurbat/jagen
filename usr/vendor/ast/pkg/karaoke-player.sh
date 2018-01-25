#!/bin/sh

jagen_pkg_configure_host() {
    pkg_configure \
        $(pkg_using_target_board)
}
