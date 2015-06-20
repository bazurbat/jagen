#!/bin/sh

pkg_build() {
    p_run ./configure \
        --host="$p_system" \
        --prefix="$p_prefix"

    p_run make
}
