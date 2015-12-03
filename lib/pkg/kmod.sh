#!/bin/sh

use_toolchain target

pkg_prefix=""
pkg_dest_dir="$jagen_sdk_rootfs_prefix"

jagen_pkg_build() {
    pkg_run ./configure \
        --host="$jagen_target_system" \
        --prefix="$pkg_prefix" \
        --disable-manpages \
        --disable-gtk-doc-html

    pkg_run make
}

jagen_pkg_install() {
    pkg_run make DESTDIR="$pkg_dest_dir" install
}
