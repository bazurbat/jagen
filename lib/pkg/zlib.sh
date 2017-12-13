#!/bin/sh

jagen_pkg_configure() {
    export CHOST="$pkg_system"
    pkg_run "$pkg_source_dir/configure" \
        --prefix="$pkg_prefix" \
        --libdir="$pkg_prefix/lib"
}

cleanup_headers() {
    pkg_run sed -i -r 's:\<(O[FN])\>:_Z_\1:g' "$@"
}

jagen_pkg_install() {
    pkg_run make DESTDIR="$pkg_staging_dir" LDCONFIG=: install
    pkg_run cleanup_headers "$pkg_staging_dir$pkg_prefix"/include/*.h
}
