#!/bin/sh

jagen_pkg_patch() {
    pkg_patch

    # https://bugs.gentoo.org/462814
    pkg_run sed -i -e 's:@toolexeclibdir@:$(libdir):g' Makefile.in
}
