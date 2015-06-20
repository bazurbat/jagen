#!/bin/sh

pkg_build() {
    p_run ./configure \
        --host="$p_system" \
        --prefix="$p_prefix" \
        --disable-static \
        --disable-enca \
        --enable-fontconfig \
        --disable-harfbuzz

    p_run make
}
