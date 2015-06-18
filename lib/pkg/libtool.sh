#!/bin/sh

use_toolchain host

pkg_patch() {
    export LIBTOOLIZE=echo

    p_run cd libltdl
    p_autoreconf
    p_run cd -
    p_autoreconf
}

pkg_build() {
    export CONFIG_SHELL=/bin/bash

    p_run ./configure \
        --prefix="$host_dir$host_prefix" \
        --disable-ltdl-install

    p_run make
}

pkg_install() {
    p_run make install
}
