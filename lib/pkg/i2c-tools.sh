#!/bin/sh

pkg_build() {
    p_run make
}

pkg_install() {
    p_run make DESTDIR="$target_dir" prefix="$target_prefix" install
}
