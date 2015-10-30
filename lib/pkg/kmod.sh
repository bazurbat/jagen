#!/bin/sh

use_toolchain target

p_prefix=""
p_dest_dir="$sdk_rootfs_prefix"

jagen_pkg_build() {
    pkg_run ./configure \
        --host="$target_system" \
        --prefix="$p_prefix" \
        --disable-manpages \
        --disable-gtk-doc-html

    pkg_run make
}

jagen_pkg_install() {
    pkg_run make DESTDIR="$p_dest_dir" install
}
