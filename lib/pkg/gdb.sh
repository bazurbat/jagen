#!/bin/sh

jagen_pkg_configure_host() {
    pkg_run "$pkg_source_dir/configure" \
        --target="$jagen_target_system" \
        --prefix="$jagen_host_dir" \
        --program-transform-name='' \
        --disable-werror \
        --disable-binutils \
        --disable-etc \
        --disable-gas \
        --disable-gold \
        --disable-gprof \
        --disable-ld \
        --disable-gdbserver \
        --disable-readline \
        --with-system-readline \
        --with-expat \
        --with-python="/usr/bin/python2.7" \
        --with-zlib
}

jagen_pkg_install_host() {
    pkg_run make install
}
