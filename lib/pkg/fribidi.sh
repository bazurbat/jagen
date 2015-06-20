#!/bin/sh

pkg_build() {
    p_run ./configure \
        --host="$p_system" \
        --prefix="$p_prefix" \
        --with-glib=no

    p_run make
}
