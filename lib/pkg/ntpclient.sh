#!/bin/sh

jagen_pkg_patch() {
    pkg_run sed -ri 's|^(CFLAGS.*ENABLE_DEBUG)|# \1|' Makefile
    pkg_run sed -ri 's|^(CFLAGS.*ENABLE_REPLAY)|# \1|' Makefile
}

jagen_pkg_install() {
    local dest="$pkg_install_root/bin"
    pkg_run install -vd "$dest"
    pkg_run install -vm755 ntpclient "$dest"
}
