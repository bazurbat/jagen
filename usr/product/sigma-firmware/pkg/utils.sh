#!/bin/sh

jagen_pkg_configure_host() {
    pkg_configure \
        -DUSE_LOOPAES=NO
}

jagen_pkg_configure_target() {
    pkg_configure \
        -DUSE_LOOPAES=YES
}
