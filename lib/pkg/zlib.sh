#!/bin/sh

pkg_build() {
    p_run ./configure \
        --prefix="$p_prefix" \
        --libdir="$p_prefix/lib"

    p_run make
}

cleanup_headers() {
    p_run sed -i -r 's:\<(O[FN])\>:_Z_\1:g' "$@"
}

pkg_install() {
    p_run make DESTDIR="$p_dest_dir" LDCONFIG=: install
    p_run cleanup_headers "$p_dest_dir$p_prefix"/include/*.h
}
