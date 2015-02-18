#!/bin/sh

use_toolchain target

p_prefix=""
p_dest_dir="$sdk_rootfs_prefix"

pkg_build() {
    p_run ./configure \
        --host="$target_system" \
        --prefix="$p_prefix" \
        --disable-manpages \
        --disable-gtk-doc-html

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$p_dest_dir" install
}
