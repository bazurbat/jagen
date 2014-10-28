#!/bin/sh

psource="soundtouch-1.8.0"
psourcedir="$pworkdir/soundtouch"

use_env target

pkg_prepare() {
    p_cmd ./bootstrap
}

pkg_build() {
    p_cmd ./configure \
        --host="mipsel-linux" \
        --prefix="$targetprefix" \
        --enable-integer-samples=yes \
        --enable-x86-optimizations=no

    p_make
}

pkg_install() {
    p_make DESTDIR="$targetdir" install
}
