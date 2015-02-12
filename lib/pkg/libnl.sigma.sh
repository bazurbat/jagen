#!/bin/sh

use_toolchain target

p_prefix="$target_prefix"
p_dest_dir="$target_dir"

pkg_build() {
    p_run ./configure \
        --host="$target_system" \
        --prefix="$p_prefix" \
        --enable-shared \
        --disable-static \
        --disable-cli

    p_run make
}

pkg_install() {
    local la="nl-3 nl-genl-3 nl-route-3 nl-nf-3 nl-idiag-3"

    p_run make DESTDIR="$p_dest_dir" install

    for f in $la; do
        p_fix_la "$p_dest_dir$p_prefix/lib/lib${f}.la" "$p_dest_dir"
    done
}
