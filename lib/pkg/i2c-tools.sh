#!/bin/sh

pkg_build() {
    pkg_run make
}

pkg_install() {
    pkg_run make DESTDIR="$jagen_target_dir" prefix="$jagen_target_prefix" install
}
