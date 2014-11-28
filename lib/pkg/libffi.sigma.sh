#!/bin/sh

p_source="$pkg_dist_dir/libffi-3.1.tar.gz"

use_toolchain target

pkg_patch() {
    p_run patch -p0 -i \
        "$pkg_dist_dir"/patches/libffi-3.1-execstack.patch
    p_run patch -p0 -i \
        "$pkg_dist_dir"/patches/libffi-3.1-typing_error.patch

    p_run sed -i -e 's:@toolexeclibdir@:$(libdir):g' Makefile.in
    # http://sourceware.org/ml/libffi-discuss/2014/msg00060.html
    p_run sed -i -e 's:@toolexeclibdir@:${libdir}:' libffi.pc.in
}

pkg_build() {
    # CFLAGS="$CFLAGS -mno-compact-eh"

    p_run ./configure \
        --host="$target_system" \
        --prefix="$target_prefix"

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$target_dir" install
}
