#!/bin/sh

use_env tools

pkg_build_host() {
    p_run ./configure \
        --target="$target_system" \
        --prefix="$tools_dir" \
        --program-transform-name='' \
        --disable-nls \
        --with-system-readline \
        --with-expat \
        --with-python="/usr/bin/python2.7" \
        --with-zlib

    p_run make
}

pkg_install_host() {
    p_run make install
}
