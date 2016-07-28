#!/bin/sh

jagen_pkg_configure() {
    pkg_configure

    if in_flags devenv; then
        pkg_run sed -ri 's/^CONFIG_INITRAMFS_SOURCE=".*"$/CONFIG_INITRAMFS_SOURCE=""/' .config
    fi
}
