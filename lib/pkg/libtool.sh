#!/bin/sh

jagen_pkg_patch() {
    pkg_patch

    export LIBTOOLIZE=echo

    pkg_run autoreconf -ifv libltdl
    pkg_run autoreconf -ifv
}

jagen_pkg_configure() {
    export CONFIG_SHELL=/bin/bash

    pkg_configure
}
