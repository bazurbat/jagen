#!/bin/sh

jagen_pkg_patch() {
    default_patch

    pkg_run sed -i -e 's:@toolexeclibdir@:$(libdir):g' Makefile.in
    # http://sourceware.org/ml/libffi-discuss/2014/msg00060.html
    pkg_run sed -i -e 's:@toolexeclibdir@:${libdir}:' libffi.pc.in
}

jagen_pkg_build() {
    # Needed starting from GCC 4.4
    CFLAGS="$CFLAGS -mno-compact-eh"
    default_build
}
