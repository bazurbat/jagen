#!/bin/sh

use_toolchain host

jagen_pkg_patch() {
    export LIBTOOLIZE=echo

    pkg_run cd libltdl
    pkg_run_autoreconf
    pkg_run cd -
    pkg_run_autoreconf
}

jagen_pkg_build() {
    export CONFIG_SHELL=/bin/bash

    pkg_run ./configure \
        --prefix="$jagen_host_dir$jagen_host_prefix" \
        --disable-ltdl-install

    pkg_run make
}

jagen_pkg_install() {
    pkg_run make install
}
