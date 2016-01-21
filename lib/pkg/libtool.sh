#!/bin/sh

use_env host

jagen_pkg_patch() {
    default_patch

    export LIBTOOLIZE=echo

    pkg_run cd libltdl
    pkg_run_autoreconf
    pkg_run cd -
    pkg_run_autoreconf
}

jagen_pkg_build() {
    export CONFIG_SHELL=/bin/bash

    pkg_run "$pkg_source_dir/configure" \
        --prefix="$jagen_host_dir$jagen_host_prefix" \
        --disable-ltdl-install

    pkg_run make
}

jagen_pkg_install() {
    pkg_run make install
}
