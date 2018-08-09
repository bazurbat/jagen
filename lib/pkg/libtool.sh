#!/bin/sh

jagen_stage_patch() {
    pkg_patch

    export LIBTOOLIZE=echo

    pkg_run autoreconf -ifv libltdl
    pkg_run autoreconf -ifv
}

jagen_stage_configure() {
    export CONFIG_SHELL=/bin/bash

    pkg_configure
}
