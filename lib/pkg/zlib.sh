#!/bin/sh

jagen_pkg_build() {
    pkg_run ./configure \
        --prefix="$p_prefix" \
        --libdir="$p_prefix/lib"

    pkg_run make
}

cleanup_headers() {
    pkg_run sed -i -r 's:\<(O[FN])\>:_Z_\1:g' "$@"
}

jagen_pkg_install() {
    pkg_run make DESTDIR="$p_dest_dir" LDCONFIG=: install
    pkg_run cleanup_headers "$p_dest_dir$p_prefix"/include/*.h
}
