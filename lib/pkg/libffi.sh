#!/bin/sh

pkg_patch() {
    p_run sed -i -e 's:@toolexeclibdir@:$(libdir):g' Makefile.in
    # http://sourceware.org/ml/libffi-discuss/2014/msg00060.html
    p_run sed -i -e 's:@toolexeclibdir@:${libdir}:' libffi.pc.in
}

pkg_build() {
    in_flags "new_toolchain" && CFLAGS="$CFLAGS -mno-compact-eh"
    default_build
}
