#!/bin/sh

jagen_pkg_configure_host() {
    pkg_configure \
        -DTARGET_BOARD="$jagen_target_board"
}
