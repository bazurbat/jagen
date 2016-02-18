#!/bin/sh

jagen_pkg_configure() {
    pkg_run "$pkg_source_dir/configure" \
        --prefix="$pkg_prefix" \
        --libdir="$pkg_prefix/lib"
}

cleanup_headers() {
    pkg_run sed -i -r 's:\<(O[FN])\>:_Z_\1:g' "$@"
}

jagen_pkg_install() {
    pkg_run make DESTDIR="$pkg_sysroot" LDCONFIG=: install
    pkg_run cleanup_headers "$pkg_sysroot$pkg_prefix"/include/*.h
}
