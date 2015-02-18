#!/bin/sh

use_toolchain target

p_dest_dir="$sdk_rootfs_prefix"

pkg_build() {
    local include_dir="$sdk_rootfs_prefix/include"
    local lib_dir="$sdk_rootfs_prefix/lib"

    export BLKID_CFLAGS="-I$include_dir"
    export BLKID_LIBS="-L$lib_dir -lblkid"
    export KMOD_CFLAGS="-I$include_dir"
    export KMOD_LIBS="-L$lib_dir -lkmod"

    p_run ./configure \
        --host="$target_system" \
        --prefix="" \
        --disable-gtk-doc-html \
        --disable-manpages \
        --disable-gudev \
        --disable-introspection \
        --disable-keymap \
        --disable-mtd_probe \
        --disable-rule_generator \
        --disable-floppy

    p_run make
}

pkg_install() {
    p_run make DESTDIR="$p_dest_dir" install
}
