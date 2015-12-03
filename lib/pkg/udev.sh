#!/bin/sh

use_toolchain target

pkg_dest_dir="$sdk_rootfs_prefix"

jagen_pkg_build() {
    local include_dir="$sdk_rootfs_prefix/include"
    local lib_dir="$sdk_rootfs_prefix/lib"

    export BLKID_CFLAGS="-I$include_dir"
    export BLKID_LIBS="-L$lib_dir -lblkid"
    export KMOD_CFLAGS="-I$include_dir"
    export KMOD_LIBS="-L$lib_dir -lkmod"

    pkg_run ./configure \
        --host="$jagen_target_system" \
        --prefix="" \
        --disable-gtk-doc-html \
        --disable-manpages \
        --disable-gudev \
        --disable-introspection \
        --disable-keymap \
        --disable-mtd_probe \
        --disable-rule_generator \
        --disable-floppy

    pkg_run make
}

jagen_pkg_install() {
    pkg_run make DESTDIR="$pkg_dest_dir" install
}
