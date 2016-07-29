#!/bin/sh

jagen_pkg_compile() {
    pkg_run mkdir -vp \
        bin \
        etc \
        include \
        lib \
        libexec \
        sbin \
        share \
        var \
        var/lib \
        var/log
    pkg_run ln -vsnf ../run run
    pkg_run ln -vsnf ../../run var/run
    pkg_run ln -vsnf ../../tmp var/tmp
}
