#!/bin/sh

psource="soundtouch-1.8.0"
psourcedir="$pworkdir/soundtouch"

use_env target

pkg_prepare() {
    p_run ./bootstrap
}

pkg_build() {
    p_run ./configure \
        --host="mipsel-linux" \
        --prefix="$targetprefix" \
        --enable-integer-samples=yes \
        --enable-x86-optimizations=no

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$targetdir" install
}
