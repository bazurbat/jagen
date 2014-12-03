#!/bin/sh

p_source="$pkg_dist_dir/gdb-7.6.2.tar.bz2"

use_toolchain target

workdir="gdb/gdbserver"

pkg_build() {
    p_run cd "$workdir"
    p_run ./configure \
        --host="$target_system" \
        --prefix=""

    p_run make
}

pkg_install() {
    p_run cd "$workdir"
    p_run make DESTDIR="$sdk_rootfs_prefix" install
}
