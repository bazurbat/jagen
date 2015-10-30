#!/bin/sh

pkg_build() {
    p_run make
}

pkg_install() {
    p_run make DESTDIR="$jagen_target_dir" prefix="$jagen_target_prefix" install
}
