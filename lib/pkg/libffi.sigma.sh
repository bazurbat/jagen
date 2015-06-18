#!/bin/sh

use_toolchain target

p_prefix="$target_prefix"
p_dest_dir="$target_dir"

pkg_patch() {
    p_run sed -i -e 's:@toolexeclibdir@:$(libdir):g' Makefile.in
    # http://sourceware.org/ml/libffi-discuss/2014/msg00060.html
    p_run sed -i -e 's:@toolexeclibdir@:${libdir}:' libffi.pc.in
}

pkg_build() {
    in_flags "new_toolchain" && CFLAGS="$CFLAGS -mno-compact-eh"

    p_run ./configure \
        --host="$target_system" \
        --prefix="$p_prefix"

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$p_dest_dir" install
    p_fix_la "$p_dest_dir$p_prefix/lib/libffi.la" "$p_dest_dir"
}
